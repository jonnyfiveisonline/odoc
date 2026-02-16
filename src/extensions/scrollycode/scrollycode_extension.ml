(** Scrollycode Extension for odoc

    Provides scroll-driven code tutorials with three visual themes:
    - warm: Earthy, bookish aesthetic (Fraunces + Source Serif)
    - dark: Cinematic terminal aesthetic (JetBrains Mono + Outfit)
    - notebook: Clean editorial aesthetic (Newsreader + DM Sans)

    Authoring format uses @scrolly.<theme> custom tags with an ordered
    list inside, where each list item is a tutorial step containing
    a bold title, prose paragraphs, and a code block. *)

module Comment = Odoc_model.Comment
module Location_ = Odoc_model.Location_
module Block = Odoc_document.Types.Block
module Inline = Odoc_document.Types.Inline

(** {1 Step Extraction} *)

(** A single tutorial step extracted from the ordered list structure *)
type step = {
  title : string;
  prose : string;
  code : string;
  focus : int list; (** 1-based line numbers to highlight *)
}

(** Extract plain text from inline elements *)
let rec text_of_inline (el : Comment.inline_element Location_.with_location) =
  match el.Location_.value with
  | `Space -> " "
  | `Word w -> w
  | `Code_span c -> "`" ^ c ^ "`"
  | `Math_span m -> m
  | `Raw_markup (_, r) -> r
  | `Styled (_, content) -> text_of_inlines content
  | `Reference (_, content) -> text_of_link_content content
  | `Link (_, content) -> text_of_link_content content

and text_of_inlines content =
  String.concat "" (List.map text_of_inline content)

and text_of_link_content content =
  String.concat "" (List.map text_of_non_link content)

and text_of_non_link
    (el : Comment.non_link_inline_element Location_.with_location) =
  match el.Location_.value with
  | `Space -> " "
  | `Word w -> w
  | `Code_span c -> "`" ^ c ^ "`"
  | `Math_span m -> m
  | `Raw_markup (_, r) -> r
  | `Styled (_, content) -> text_of_link_content content

let text_of_paragraph (p : Comment.paragraph) =
  String.concat "" (List.map text_of_inline p)

(** Extract title, prose, code and focus lines from a single list item *)
let extract_step
    (item : Comment.nestable_block_element Location_.with_location list) : step
    =
  let title = ref "" in
  let prose_parts = ref [] in
  let code = ref "" in
  let focus = ref [] in
  List.iter
    (fun (el : Comment.nestable_block_element Location_.with_location) ->
      match el.Location_.value with
      | `Paragraph p -> (
          let text = text_of_paragraph p in
          (* Check if the paragraph starts with bold text — that's the title *)
          match p with
          | first :: _
            when (match first.Location_.value with
                 | `Styled (`Bold, _) -> true
                 | _ -> false) ->
              if !title = "" then title := text
              else prose_parts := text :: !prose_parts
          | _ -> prose_parts := text :: !prose_parts)
      | `Code_block { content = code_content; _ } ->
          let code_text = code_content.Location_.value in
          (* Check for focus annotation in the code: lines starting with >>> *)
          let lines = String.split_on_char '\n' code_text in
          let focused_lines = ref [] in
          let clean_lines =
            List.mapi
              (fun i line ->
                if
                  String.length line >= 4
                  && String.sub line 0 4 = "(* >"
                then (
                  focused_lines := (i + 1) :: !focused_lines;
                  (* Remove the focus marker *)
                  let rest = String.sub line 4 (String.length line - 4) in
                  let rest =
                    if
                      String.length rest >= 4
                      && String.sub rest (String.length rest - 4) 4 = "< *)"
                    then String.sub rest 0 (String.length rest - 4)
                    else rest
                  in
                  String.trim rest)
                else line)
              lines
          in
          code := String.concat "\n" clean_lines;
          focus := List.rev !focused_lines
      | `Verbatim v -> prose_parts := v :: !prose_parts
      | _ -> ())
    item;
  {
    title = !title;
    prose = String.concat "\n\n" (List.rev !prose_parts);
    code = !code;
    focus = !focus;
  }

(** Extract all steps from the tag content (expects an ordered list) *)
let extract_steps
    (content :
      Comment.nestable_block_element Location_.with_location list) :
    string * step list =
  (* First element might be a paragraph with the tutorial title *)
  let tutorial_title = ref "Tutorial" in
  let steps = ref [] in
  List.iter
    (fun (el : Comment.nestable_block_element Location_.with_location) ->
      match el.Location_.value with
      | `Paragraph p ->
          let text = text_of_paragraph p in
          if !steps = [] then tutorial_title := text
      | `List (`Ordered, items) ->
          steps := List.map extract_step items
      | _ -> ())
    content;
  (!tutorial_title, !steps)

(** {1 HTML Escaping} *)

let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(** {1 OCaml Syntax Highlighting}

    A simple lexer-based highlighter for OCaml code. Produces HTML spans
    with classes for keywords, types, strings, comments, operators. *)

let ocaml_keywords =
  [
    "let"; "in"; "if"; "then"; "else"; "match"; "with"; "fun"; "function";
    "type"; "module"; "struct"; "sig"; "end"; "open"; "include"; "val";
    "rec"; "and"; "of"; "when"; "as"; "begin"; "do"; "done"; "for"; "to";
    "while"; "downto"; "try"; "exception"; "raise"; "mutable"; "ref";
    "true"; "false"; "assert"; "failwith"; "not";
  ]

let ocaml_types =
  [
    "int"; "float"; "string"; "bool"; "unit"; "list"; "option"; "array";
    "char"; "bytes"; "result"; "exn"; "ref";
  ]

(** Tokenize and highlight OCaml code into HTML *)
let highlight_ocaml code =
  let len = String.length code in
  let buf = Buffer.create (len * 2) in
  let i = ref 0 in
  let peek () = if !i < len then Some code.[!i] else None in
  let advance () = incr i in
  let current () = code.[!i] in
  while !i < len do
    match current () with
    (* Comments *)
    | '(' when !i + 1 < len && code.[!i + 1] = '*' ->
        Buffer.add_string buf "<span class=\"hl-comment\">";
        Buffer.add_string buf "(*";
        i := !i + 2;
        let depth = ref 1 in
        while !depth > 0 && !i < len do
          if !i + 1 < len && code.[!i] = '(' && code.[!i + 1] = '*' then (
            Buffer.add_string buf "(*";
            i := !i + 2;
            incr depth)
          else if !i + 1 < len && code.[!i] = '*' && code.[!i + 1] = ')' then (
            Buffer.add_string buf "*)";
            i := !i + 2;
            decr depth)
          else (
            Buffer.add_string buf (html_escape (String.make 1 code.[!i]));
            advance ())
        done;
        Buffer.add_string buf "</span>"
    (* Strings *)
    | '"' ->
        Buffer.add_string buf "<span class=\"hl-string\">";
        Buffer.add_char buf '"';
        advance ();
        while !i < len && current () <> '"' do
          if current () = '\\' && !i + 1 < len then (
            Buffer.add_string buf (html_escape (String.make 1 (current ())));
            advance ();
            Buffer.add_string buf (html_escape (String.make 1 (current ())));
            advance ())
          else (
            Buffer.add_string buf (html_escape (String.make 1 (current ())));
            advance ())
        done;
        if !i < len then (
          Buffer.add_char buf '"';
          advance ());
        Buffer.add_string buf "</span>"
    (* Char literals *)
    | '\'' when !i + 2 < len && code.[!i + 2] = '\'' ->
        Buffer.add_string buf "<span class=\"hl-string\">";
        Buffer.add_char buf '\'';
        advance ();
        Buffer.add_string buf (html_escape (String.make 1 (current ())));
        advance ();
        Buffer.add_char buf '\'';
        advance ();
        Buffer.add_string buf "</span>"
    (* Numbers *)
    | '0' .. '9' ->
        Buffer.add_string buf "<span class=\"hl-number\">";
        while
          !i < len
          &&
          match current () with
          | '0' .. '9' | '.' | '_' | 'x' | 'o' | 'b' | 'a' .. 'f'
          | 'A' .. 'F' ->
              true
          | _ -> false
        do
          Buffer.add_char buf (current ());
          advance ()
        done;
        Buffer.add_string buf "</span>"
    (* Identifiers and keywords *)
    | 'a' .. 'z' | '_' ->
        let start = !i in
        while
          !i < len
          &&
          match current () with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
          | _ -> false
        do
          advance ()
        done;
        let word = String.sub code start (!i - start) in
        if List.mem word ocaml_keywords then
          Buffer.add_string buf
            (Printf.sprintf "<span class=\"hl-keyword\">%s</span>"
               (html_escape word))
        else if List.mem word ocaml_types then
          Buffer.add_string buf
            (Printf.sprintf "<span class=\"hl-type\">%s</span>"
               (html_escape word))
        else Buffer.add_string buf (html_escape word)
    (* Module/constructor names (capitalized identifiers) *)
    | 'A' .. 'Z' ->
        let start = !i in
        while
          !i < len
          &&
          match current () with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
          | _ -> false
        do
          advance ()
        done;
        let word = String.sub code start (!i - start) in
        Buffer.add_string buf
          (Printf.sprintf "<span class=\"hl-module\">%s</span>"
             (html_escape word))
    (* Operators *)
    | '|' | '-' | '+' | '*' | '/' | '=' | '<' | '>' | '@' | '^' | '~'
    | '!' | '?' | '%' | '&' ->
        Buffer.add_string buf "<span class=\"hl-operator\">";
        Buffer.add_string buf (html_escape (String.make 1 (current ())));
        advance ();
        (* Consume multi-char operators *)
        while
          !i < len
          &&
          match current () with
          | '|' | '-' | '+' | '*' | '/' | '=' | '<' | '>' | '@' | '^'
          | '~' | '!' | '?' | '%' | '&' ->
              true
          | _ -> false
        do
          Buffer.add_string buf (html_escape (String.make 1 (current ())));
          advance ()
        done;
        Buffer.add_string buf "</span>"
    (* Punctuation *)
    | ':' | ';' | '.' | ',' | '[' | ']' | '{' | '}' | '(' | ')' ->
        Buffer.add_string buf
          (Printf.sprintf "<span class=\"hl-punct\">%s</span>"
             (html_escape (String.make 1 (current ()))));
        advance ()
    (* Arrow special case: -> *)
    | ' ' | '\t' | '\n' | '\r' ->
        Buffer.add_char buf (current ());
        advance ()
    | _ ->
        let _ = peek () in
        Buffer.add_string buf (html_escape (String.make 1 (current ())));
        advance ()
  done;
  Buffer.contents buf

(** {1 Shared JavaScript}

    The scrollycode runtime handles IntersectionObserver-based step
    detection and line-level transition animations. *)

let shared_js =
  {|
(function() {
  'use strict';

  function initScrollycode(container) {
    var steps = container.querySelectorAll('.sc-step');
    var codeBody = container.querySelector('.sc-code-body');
    var stepBadge = container.querySelector('.sc-step-badge');
    var pips = container.querySelectorAll('.sc-pip');
    var currentStep = -1;

    function parseLines(el) {
      if (!el) return [];
      var items = el.querySelectorAll('.sc-line');
      return Array.from(items).map(function(line) {
        return { id: line.dataset.id, html: line.innerHTML, focused: line.classList.contains('sc-focused') };
      });
    }

    function renderStep(index) {
      if (index === currentStep || index < 0 || index >= steps.length) return;

      var stepEl = steps[index];
      var codeSlot = stepEl.querySelector('.sc-code-slot');
      var newLines = parseLines(codeSlot);
      var oldLines = parseLines(codeBody);
      var oldById = {};
      oldLines.forEach(function(l) { oldById[l.id] = l; });
      var newById = {};
      newLines.forEach(function(l) { newById[l.id] = l; });

      // Determine exiting lines
      var exiting = oldLines.filter(function(l) { return !newById[l.id]; });

      // Animate exit
      exiting.forEach(function(l, i) {
        var el = codeBody.querySelector('[data-id="' + l.id + '"]');
        if (el) {
          el.style.animationDelay = (i * 30) + 'ms';
          el.classList.add('sc-exiting');
        }
      });

      var exitTime = exiting.length > 0 ? 200 + exiting.length * 30 : 0;

      setTimeout(function() {
        // Rebuild DOM
        codeBody.innerHTML = '';
        newLines.forEach(function(l, i) {
          var div = document.createElement('div');
          div.className = 'sc-line' + (l.focused ? ' sc-focused' : '') + (!oldById[l.id] ? ' sc-entering' : '');
          div.dataset.id = l.id;
          div.innerHTML = '<span class="sc-line-number">' + (i + 1) + '</span>' + l.html;
          if (!oldById[l.id]) {
            div.style.animationDelay = (i * 25) + 'ms';
          }
          codeBody.appendChild(div);
        });

        // Update badge and pips
        if (stepBadge) stepBadge.textContent = (index + 1) + ' / ' + steps.length;
        pips.forEach(function(pip, i) {
          pip.classList.toggle('sc-active', i === index);
        });
      }, exitTime);

      currentStep = index;
    }

    // Set up IntersectionObserver
    var observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var idx = parseInt(entry.target.dataset.stepIndex, 10);
          renderStep(idx);
        }
      });
    }, {
      rootMargin: '-30% 0px -30% 0px',
      threshold: 0
    });

    steps.forEach(function(step) { observer.observe(step); });

    // Initialize first step
    renderStep(0);
  }

  // Initialize all scrollycode containers on the page
  document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('.sc-container').forEach(initScrollycode);
  });
})();
|}

(** {1 Theme: Warm Workshop}

    Earthy, bookish. Cream background, burnt sienna accents.
    Fraunces display + Source Serif 4 body.
    Dark navy code panel with warm syntax highlighting. *)

let warm_css =
  {|
.sc-container.sc-warm {
  --sc-bg: #f5f0e6;
  --sc-text: #2c2416;
  --sc-text-dim: #8a7c6a;
  --sc-accent: #c25832;
  --sc-accent-soft: rgba(194, 88, 50, 0.08);
  --sc-code-bg: #1a1a2e;
  --sc-code-text: #d4d0c8;
  --sc-code-gutter: #3a3a52;
  --sc-border: rgba(44, 36, 22, 0.1);
  --sc-focus-bg: rgba(194, 88, 50, 0.06);
  --sc-panel-radius: 12px;
  font-family: 'Source Serif 4', Georgia, serif;
}

.sc-container.sc-warm .sc-hero {
  background: var(--sc-bg);
  text-align: center;
  padding: 5rem 2rem 3rem;
  border-bottom: 1px solid var(--sc-border);
}

.sc-container.sc-warm .sc-hero h1 {
  font-family: 'Fraunces', serif;
  font-size: clamp(2.2rem, 5vw, 3.4rem);
  font-weight: 800;
  font-style: italic;
  color: var(--sc-text);
  letter-spacing: -0.03em;
  line-height: 1.1;
  margin-bottom: 0.75rem;
}

.sc-container.sc-warm .sc-hero p {
  color: var(--sc-text-dim);
  font-size: 1.05rem;
  max-width: 48ch;
  margin: 0 auto;
  line-height: 1.6;
}

.sc-container.sc-warm .sc-tutorial {
  display: flex;
  gap: 0;
  background: var(--sc-bg);
  position: relative;
}

.sc-container.sc-warm .sc-steps-col {
  flex: 1;
  min-width: 0;
  padding: 2rem 2.5rem 50vh 2.5rem;
}

.sc-container.sc-warm .sc-code-col {
  width: 52%;
  flex-shrink: 0;
}

.sc-container.sc-warm .sc-step {
  min-height: 70vh;
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem 0;
}

.sc-container.sc-warm .sc-step-number {
  font-family: 'Source Code Pro', monospace;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.1em;
  color: var(--sc-accent);
  text-transform: uppercase;
  margin-bottom: 0.5rem;
}

.sc-container.sc-warm .sc-step h2 {
  font-family: 'Fraunces', serif;
  font-size: 1.5rem;
  font-weight: 700;
  color: var(--sc-text);
  letter-spacing: -0.02em;
  margin-bottom: 0.75rem;
  line-height: 1.25;
}

.sc-container.sc-warm .sc-step p {
  color: var(--sc-text-dim);
  font-size: 0.95rem;
  line-height: 1.7;
  max-width: 44ch;
}

.sc-container.sc-warm .sc-code-panel {
  position: sticky;
  top: 10vh;
  height: 80vh;
  margin: 0 2rem 0 0;
  background: var(--sc-code-bg);
  border-radius: var(--sc-panel-radius);
  overflow: hidden;
  display: flex;
  flex-direction: column;
  box-shadow: 0 20px 60px rgba(26, 26, 46, 0.3), 0 0 0 1px rgba(255,255,255,0.03) inset;
}

.sc-container.sc-warm .sc-code-header {
  display: flex;
  align-items: center;
  padding: 0.85rem 1.25rem;
  background: rgba(255,255,255,0.03);
  border-bottom: 1px solid rgba(255,255,255,0.06);
  gap: 0.6rem;
}

.sc-container.sc-warm .sc-dots {
  display: flex;
  gap: 6px;
}

.sc-container.sc-warm .sc-dots span {
  width: 10px;
  height: 10px;
  border-radius: 50%;
}

.sc-container.sc-warm .sc-dots span:nth-child(1) { background: #ff5f57; }
.sc-container.sc-warm .sc-dots span:nth-child(2) { background: #ffbd2e; }
.sc-container.sc-warm .sc-dots span:nth-child(3) { background: #28c840; }

.sc-container.sc-warm .sc-filename {
  font-family: 'Source Code Pro', monospace;
  font-size: 0.72rem;
  color: rgba(255,255,255,0.35);
  letter-spacing: 0.04em;
  flex: 1;
  text-align: center;
}

.sc-container.sc-warm .sc-step-badge {
  font-family: 'Source Code Pro', monospace;
  font-size: 0.65rem;
  color: rgba(255,255,255,0.25);
  letter-spacing: 0.06em;
}

.sc-container.sc-warm .sc-code-body {
  flex: 1;
  overflow-y: auto;
  padding: 1.25rem 0;
  font-family: 'Source Code Pro', monospace;
  font-size: 0.82rem;
  line-height: 1.7;
  color: var(--sc-code-text);
}

.sc-container.sc-warm .sc-line {
  padding: 0 1.25rem;
  white-space: pre;
  transition: opacity 0.3s ease;
  opacity: 0.35;
}

.sc-container.sc-warm .sc-line.sc-focused {
  opacity: 1;
  background: rgba(194, 88, 50, 0.06);
}

.sc-container.sc-warm .sc-line-number {
  display: inline-block;
  width: 3ch;
  text-align: right;
  margin-right: 1.5ch;
  color: var(--sc-code-gutter);
  user-select: none;
}

/* Syntax highlighting */
.sc-container.sc-warm .hl-keyword { color: #f0a6a0; font-weight: 500; }
.sc-container.sc-warm .hl-type { color: #8ec8e8; }
.sc-container.sc-warm .hl-string { color: #b8d89a; }
.sc-container.sc-warm .hl-comment { color: #6a6a82; font-style: italic; }
.sc-container.sc-warm .hl-number { color: #ddb97a; }
.sc-container.sc-warm .hl-module { color: #e8c87a; }
.sc-container.sc-warm .hl-operator { color: #c8a8d8; }
.sc-container.sc-warm .hl-punct { color: #7a7a92; }

/* Progress pips */
.sc-container.sc-warm .sc-progress {
  position: fixed;
  left: 1.5rem;
  top: 50%;
  transform: translateY(-50%);
  display: flex;
  flex-direction: column;
  gap: 8px;
  z-index: 100;
}

.sc-container.sc-warm .sc-pip {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background: var(--sc-border);
  transition: all 0.3s ease;
}

.sc-container.sc-warm .sc-pip.sc-active {
  background: var(--sc-accent);
  box-shadow: 0 0 8px rgba(194, 88, 50, 0.4);
  transform: scale(1.4);
}

/* Animations */
@keyframes sc-line-exit {
  0% { opacity: 1; transform: translateX(0); }
  100% { opacity: 0; transform: translateX(-30px); }
}

@keyframes sc-line-enter {
  0% { opacity: 0; transform: translateX(30px); }
  100% { opacity: 1; transform: translateX(0); }
}

.sc-container.sc-warm .sc-line.sc-exiting {
  animation: sc-line-exit 0.2s cubic-bezier(0.22, 1, 0.36, 1) forwards;
}

.sc-container.sc-warm .sc-line.sc-entering {
  animation: sc-line-enter 0.25s cubic-bezier(0.22, 1, 0.36, 1) both;
}

/* Hidden code slot */
.sc-code-slot { display: none; }
|}

(** {1 Theme: Dark Terminal}

    Cinematic dark theme. Near-black background, phosphor green and amber.
    JetBrains Mono + Outfit geometric sans.
    Code panel is hero-sized, prose is a narrow overlay strip. *)

let dark_css =
  {|
.sc-container.sc-dark {
  --sc-bg: #0a0a0f;
  --sc-text: #e8e6f0;
  --sc-text-dim: #6e6b80;
  --sc-accent: #4ade80;
  --sc-accent-alt: #fbbf24;
  --sc-code-bg: #0f0f18;
  --sc-code-text: #c8c5d8;
  --sc-code-gutter: #2a2a3e;
  --sc-border: rgba(255, 255, 255, 0.06);
  --sc-panel-radius: 0;
  font-family: 'Outfit', sans-serif;
  background: var(--sc-bg);
  color: var(--sc-text);
}

.sc-container.sc-dark .sc-hero {
  background: var(--sc-bg);
  text-align: left;
  padding: 8rem 4rem 4rem;
  max-width: 800px;
  position: relative;
}

.sc-container.sc-dark .sc-hero::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: radial-gradient(ellipse at 20% 50%, rgba(74, 222, 128, 0.04) 0%, transparent 60%);
  pointer-events: none;
}

.sc-container.sc-dark .sc-hero h1 {
  font-family: 'Outfit', sans-serif;
  font-size: clamp(2.8rem, 6vw, 4.5rem);
  font-weight: 800;
  color: var(--sc-text);
  letter-spacing: -0.04em;
  line-height: 1.0;
  margin-bottom: 1.25rem;
}

.sc-container.sc-dark .sc-hero h1 em {
  font-style: normal;
  color: var(--sc-accent);
}

.sc-container.sc-dark .sc-hero p {
  color: var(--sc-text-dim);
  font-size: 1.1rem;
  max-width: 50ch;
  line-height: 1.6;
  font-weight: 300;
}

.sc-container.sc-dark .sc-tutorial {
  display: flex;
  gap: 0;
  position: relative;
}

.sc-container.sc-dark .sc-steps-col {
  width: 38%;
  flex-shrink: 0;
  padding: 2rem 2.5rem 50vh 4rem;
  border-right: 1px solid var(--sc-border);
}

.sc-container.sc-dark .sc-code-col {
  flex: 1;
  min-width: 0;
}

.sc-container.sc-dark .sc-step {
  min-height: 70vh;
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem 0;
}

.sc-container.sc-dark .sc-step-number {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.65rem;
  font-weight: 700;
  letter-spacing: 0.15em;
  color: var(--sc-accent);
  text-transform: uppercase;
  margin-bottom: 0.75rem;
  display: flex;
  align-items: center;
  gap: 0.75rem;
}

.sc-container.sc-dark .sc-step-number::after {
  content: '';
  flex: 1;
  height: 1px;
  background: var(--sc-border);
}

.sc-container.sc-dark .sc-step h2 {
  font-family: 'Outfit', sans-serif;
  font-size: 1.4rem;
  font-weight: 700;
  color: var(--sc-text);
  letter-spacing: -0.02em;
  margin-bottom: 0.75rem;
  line-height: 1.2;
}

.sc-container.sc-dark .sc-step p {
  color: var(--sc-text-dim);
  font-size: 0.9rem;
  line-height: 1.7;
  max-width: 40ch;
  font-weight: 300;
}

.sc-container.sc-dark .sc-code-panel {
  position: sticky;
  top: 0;
  height: 100vh;
  background: var(--sc-code-bg);
  display: flex;
  flex-direction: column;
  border-left: 1px solid var(--sc-border);
}

.sc-container.sc-dark .sc-code-header {
  display: flex;
  align-items: center;
  padding: 1rem 1.5rem;
  border-bottom: 1px solid var(--sc-border);
  gap: 1rem;
}

.sc-container.sc-dark .sc-dots {
  display: flex;
  gap: 6px;
}

.sc-container.sc-dark .sc-dots span {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--sc-code-gutter);
}

.sc-container.sc-dark .sc-filename {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.7rem;
  color: var(--sc-text-dim);
  letter-spacing: 0.04em;
  flex: 1;
}

.sc-container.sc-dark .sc-step-badge {
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.6rem;
  color: var(--sc-accent);
  letter-spacing: 0.08em;
  background: rgba(74, 222, 128, 0.08);
  padding: 0.25em 0.75em;
  border-radius: 3px;
}

.sc-container.sc-dark .sc-code-body {
  flex: 1;
  overflow-y: auto;
  padding: 1.5rem 0;
  font-family: 'JetBrains Mono', monospace;
  font-size: 0.8rem;
  line-height: 1.75;
  color: var(--sc-code-text);
}

.sc-container.sc-dark .sc-line {
  padding: 0 1.5rem;
  white-space: pre;
  transition: opacity 0.3s ease, background 0.3s ease;
  opacity: 0.25;
}

.sc-container.sc-dark .sc-line.sc-focused {
  opacity: 1;
  background: rgba(74, 222, 128, 0.04);
  border-left: 2px solid var(--sc-accent);
  padding-left: calc(1.5rem - 2px);
}

.sc-container.sc-dark .sc-line-number {
  display: inline-block;
  width: 3ch;
  text-align: right;
  margin-right: 2ch;
  color: var(--sc-code-gutter);
  user-select: none;
}

/* Syntax highlighting — neon palette */
.sc-container.sc-dark .hl-keyword { color: #ff7eb3; font-weight: 500; }
.sc-container.sc-dark .hl-type { color: #7dd3fc; }
.sc-container.sc-dark .hl-string { color: #4ade80; }
.sc-container.sc-dark .hl-comment { color: #4a4a62; font-style: italic; }
.sc-container.sc-dark .hl-number { color: #fbbf24; }
.sc-container.sc-dark .hl-module { color: #c4b5fd; }
.sc-container.sc-dark .hl-operator { color: #67e8f9; }
.sc-container.sc-dark .hl-punct { color: #4a4a62; }

/* Progress pips */
.sc-container.sc-dark .sc-progress {
  position: fixed;
  right: 1.5rem;
  top: 50%;
  transform: translateY(-50%);
  display: flex;
  flex-direction: column;
  gap: 10px;
  z-index: 100;
}

.sc-container.sc-dark .sc-pip {
  width: 3px;
  height: 20px;
  border-radius: 2px;
  background: var(--sc-border);
  transition: all 0.3s ease;
}

.sc-container.sc-dark .sc-pip.sc-active {
  background: var(--sc-accent);
  box-shadow: 0 0 12px rgba(74, 222, 128, 0.5);
  height: 30px;
}

/* Animations */
.sc-container.sc-dark .sc-line.sc-exiting {
  animation: sc-line-exit 0.2s cubic-bezier(0.22, 1, 0.36, 1) forwards;
}

.sc-container.sc-dark .sc-line.sc-entering {
  animation: sc-line-enter 0.25s cubic-bezier(0.22, 1, 0.36, 1) both;
}

.sc-code-slot { display: none; }
|}

(** {1 Theme: Notebook}

    Clean editorial. Soft white, blue-violet accent.
    Newsreader display + DM Sans body.
    Vertical layout with code blocks inline but sticky. *)

let notebook_css =
  {|
.sc-container.sc-notebook {
  --sc-bg: #fafbfe;
  --sc-text: #1a1a2e;
  --sc-text-dim: #64648a;
  --sc-accent: #6366f1;
  --sc-accent-soft: rgba(99, 102, 241, 0.06);
  --sc-code-bg: #1e1e32;
  --sc-code-text: #d1d0e0;
  --sc-code-gutter: #3a3a52;
  --sc-border: rgba(99, 102, 241, 0.08);
  --sc-panel-radius: 16px;
  font-family: 'DM Sans', sans-serif;
}

.sc-container.sc-notebook .sc-hero {
  background: var(--sc-bg);
  text-align: left;
  padding: 6rem 0 3rem;
  max-width: 640px;
  margin: 0 auto;
  border-bottom: 2px solid var(--sc-accent);
  position: relative;
}

.sc-container.sc-notebook .sc-hero::after {
  content: '';
  position: absolute;
  bottom: -2px;
  left: 0;
  width: 120px;
  height: 2px;
  background: var(--sc-accent);
  box-shadow: 0 0 16px rgba(99, 102, 241, 0.4);
}

.sc-container.sc-notebook .sc-hero h1 {
  font-family: 'Newsreader', serif;
  font-size: clamp(2rem, 4vw, 2.8rem);
  font-weight: 600;
  color: var(--sc-text);
  letter-spacing: -0.02em;
  line-height: 1.15;
  margin-bottom: 0.75rem;
}

.sc-container.sc-notebook .sc-hero p {
  color: var(--sc-text-dim);
  font-size: 1rem;
  max-width: 52ch;
  line-height: 1.6;
  font-weight: 400;
}

.sc-container.sc-notebook .sc-tutorial {
  display: flex;
  gap: 0;
  background: var(--sc-bg);
  max-width: 1200px;
  margin: 0 auto;
  position: relative;
}

.sc-container.sc-notebook .sc-steps-col {
  flex: 1;
  min-width: 0;
  padding: 2rem 3rem 50vh 0;
  max-width: 420px;
}

.sc-container.sc-notebook .sc-code-col {
  flex: 1;
  min-width: 0;
}

.sc-container.sc-notebook .sc-step {
  min-height: 60vh;
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 1.5rem 0;
  position: relative;
}

.sc-container.sc-notebook .sc-step::before {
  content: '';
  position: absolute;
  left: -1.5rem;
  top: 50%;
  transform: translateY(-50%);
  width: 3px;
  height: 0;
  background: var(--sc-accent);
  border-radius: 2px;
  transition: height 0.4s cubic-bezier(0.22, 1, 0.36, 1);
}

.sc-container.sc-notebook .sc-step-number {
  font-family: 'DM Sans', sans-serif;
  font-size: 0.68rem;
  font-weight: 700;
  letter-spacing: 0.12em;
  color: var(--sc-accent);
  text-transform: uppercase;
  margin-bottom: 0.5rem;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.sc-container.sc-notebook .sc-step h2 {
  font-family: 'Newsreader', serif;
  font-size: 1.3rem;
  font-weight: 600;
  color: var(--sc-text);
  letter-spacing: -0.01em;
  margin-bottom: 0.6rem;
  line-height: 1.3;
}

.sc-container.sc-notebook .sc-step p {
  color: var(--sc-text-dim);
  font-size: 0.88rem;
  line-height: 1.7;
  max-width: 42ch;
}

.sc-container.sc-notebook .sc-code-panel {
  position: sticky;
  top: 8vh;
  height: 84vh;
  margin: 0 0 0 2rem;
  background: var(--sc-code-bg);
  border-radius: var(--sc-panel-radius);
  overflow: hidden;
  display: flex;
  flex-direction: column;
  box-shadow:
    0 24px 80px rgba(30, 30, 50, 0.15),
    0 0 0 1px rgba(99, 102, 241, 0.08);
}

.sc-container.sc-notebook .sc-code-header {
  display: flex;
  align-items: center;
  padding: 0.75rem 1.25rem;
  background: rgba(99, 102, 241, 0.04);
  border-bottom: 1px solid rgba(255,255,255,0.04);
  gap: 0.75rem;
}

.sc-container.sc-notebook .sc-dots {
  display: flex;
  gap: 5px;
}

.sc-container.sc-notebook .sc-dots span {
  width: 9px;
  height: 9px;
  border-radius: 50%;
  background: rgba(255,255,255,0.08);
}

.sc-container.sc-notebook .sc-filename {
  font-family: 'DM Mono', monospace;
  font-size: 0.7rem;
  color: rgba(255,255,255,0.3);
  letter-spacing: 0.04em;
  flex: 1;
  text-align: center;
}

.sc-container.sc-notebook .sc-step-badge {
  font-family: 'DM Mono', monospace;
  font-size: 0.6rem;
  color: var(--sc-accent);
  letter-spacing: 0.06em;
}

.sc-container.sc-notebook .sc-code-body {
  flex: 1;
  overflow-y: auto;
  padding: 1.25rem 0;
  font-family: 'DM Mono', 'Source Code Pro', monospace;
  font-size: 0.78rem;
  line-height: 1.75;
  color: var(--sc-code-text);
}

.sc-container.sc-notebook .sc-line {
  padding: 0 1.25rem;
  white-space: pre;
  transition: opacity 0.3s ease;
  opacity: 0.3;
}

.sc-container.sc-notebook .sc-line.sc-focused {
  opacity: 1;
  background: rgba(99, 102, 241, 0.05);
}

.sc-container.sc-notebook .sc-line-number {
  display: inline-block;
  width: 3ch;
  text-align: right;
  margin-right: 1.5ch;
  color: var(--sc-code-gutter);
  user-select: none;
}

/* Syntax highlighting — cool tones */
.sc-container.sc-notebook .hl-keyword { color: #a78bfa; font-weight: 500; }
.sc-container.sc-notebook .hl-type { color: #67e8f9; }
.sc-container.sc-notebook .hl-string { color: #86efac; }
.sc-container.sc-notebook .hl-comment { color: #4a4a62; font-style: italic; }
.sc-container.sc-notebook .hl-number { color: #fde68a; }
.sc-container.sc-notebook .hl-module { color: #f9a8d4; }
.sc-container.sc-notebook .hl-operator { color: #93c5fd; }
.sc-container.sc-notebook .hl-punct { color: #4a4a62; }

/* Progress pips */
.sc-container.sc-notebook .sc-progress {
  position: fixed;
  left: 2rem;
  top: 50%;
  transform: translateY(-50%);
  display: flex;
  flex-direction: column;
  gap: 6px;
  z-index: 100;
}

.sc-container.sc-notebook .sc-pip {
  width: 8px;
  height: 8px;
  border-radius: 3px;
  background: var(--sc-border);
  transition: all 0.3s ease;
}

.sc-container.sc-notebook .sc-pip.sc-active {
  background: var(--sc-accent);
  box-shadow: 0 0 10px rgba(99, 102, 241, 0.4);
  border-radius: 2px;
  width: 8px;
  height: 16px;
}

/* Animations */
.sc-container.sc-notebook .sc-line.sc-exiting {
  animation: sc-line-exit 0.2s cubic-bezier(0.22, 1, 0.36, 1) forwards;
}

.sc-container.sc-notebook .sc-line.sc-entering {
  animation: sc-line-enter 0.25s cubic-bezier(0.22, 1, 0.36, 1) both;
}

.sc-code-slot { display: none; }
|}

(** {1 CSS to hide odoc chrome}

    When a scrollycode block is rendered, we want it to take over
    the page. This CSS hides the odoc navigation, breadcrumbs, etc. *)

let chrome_override_css =
  {|
/* Override odoc page chrome for scrollycode pages */
.odoc-nav, .odoc-tocs, .odoc-search { display: none !important; }
.odoc-preamble > h1, .odoc-preamble > h2, .odoc-preamble > h3 { display: none !important; }
.at-tags > li > .at-tag { display: none !important; }
.odoc-preamble, .odoc-content {
  max-width: none !important;
  padding: 0 !important;
  margin: 0 !important;
  display: block !important;
}
.at-tags {
  list-style: none !important;
  padding: 0 !important;
  margin: 0 !important;
}
.at-tags > li {
  display: block !important;
  margin: 0 !important;
  padding: 0 !important;
}
body.odoc, .odoc {
  padding: 0 !important;
  margin: 0 !important;
  max-width: none !important;
  background: inherit;
}
|}

(** {1 Google Fonts links} *)

let warm_fonts =
  {|<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Fraunces:ital,opsz,wght@0,9..144,300..900;1,9..144,300..900&family=Source+Code+Pro:ital,wght@0,300..900;1,300..900&family=Source+Serif+4:ital,opsz,wght@0,8..60,300..900;1,8..60,300..900&display=swap" rel="stylesheet">|}

let dark_fonts =
  {|<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@300..800&family=Outfit:wght@300..900&display=swap" rel="stylesheet">|}

let notebook_fonts =
  {|<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=DM+Mono:wght@300;400;500&family=DM+Sans:ital,opsz,wght@0,9..40,300..900;1,9..40,300..900&family=Newsreader:ital,opsz,wght@0,6..72,300..800;1,6..72,300..800&display=swap" rel="stylesheet">|}

(** {1 HTML Generation} *)

(** Generate the code lines HTML for a step's code slot *)
let generate_code_lines code focus =
  let lines = String.split_on_char '\n' code in
  let buf = Buffer.create 1024 in
  List.iteri
    (fun i line ->
      let line_num = i + 1 in
      let focused = focus = [] || List.mem line_num focus in
      let highlighted = highlight_ocaml line in
      Buffer.add_string buf
        (Printf.sprintf
           "<div class=\"sc-line%s\" data-id=\"L%d\">%s</div>\n"
           (if focused then " sc-focused" else "")
           line_num highlighted))
    lines;
  Buffer.contents buf

(** Generate the full scrollycode HTML for a given theme *)
let generate_html ~theme ~title ~filename steps =
  let theme_class, fonts, css =
    match theme with
    | "warm" -> ("sc-warm", warm_fonts, warm_css)
    | "dark" -> ("sc-dark", dark_fonts, dark_css)
    | "notebook" -> ("sc-notebook", notebook_fonts, notebook_css)
    | _ -> ("sc-warm", warm_fonts, warm_css)
  in
  let buf = Buffer.create 16384 in

  (* Fonts *)
  Buffer.add_string buf fonts;
  Buffer.add_char buf '\n';

  (* CSS *)
  Buffer.add_string buf "<style>\n";
  Buffer.add_string buf chrome_override_css;
  Buffer.add_string buf css;
  Buffer.add_string buf "</style>\n";

  (* Container *)
  Buffer.add_string buf
    (Printf.sprintf "<div class=\"sc-container %s\">\n" theme_class);

  (* Hero *)
  Buffer.add_string buf "<div class=\"sc-hero\">\n";
  Buffer.add_string buf
    (Printf.sprintf "  <h1>%s</h1>\n" (html_escape title));
  Buffer.add_string buf "</div>\n";

  (* Progress pips *)
  Buffer.add_string buf "<nav class=\"sc-progress\">\n";
  List.iteri
    (fun i _step ->
      Buffer.add_string buf
        (Printf.sprintf "  <div class=\"sc-pip%s\"></div>\n"
           (if i = 0 then " sc-active" else "")))
    steps;
  Buffer.add_string buf "</nav>\n";

  (* Tutorial layout *)
  Buffer.add_string buf "<div class=\"sc-tutorial\">\n";

  (* Steps column *)
  Buffer.add_string buf "  <div class=\"sc-steps-col\">\n";
  List.iteri
    (fun i step ->
      Buffer.add_string buf
        (Printf.sprintf
           "    <div class=\"sc-step\" data-step-index=\"%d\">\n" i);
      Buffer.add_string buf
        (Printf.sprintf
           "      <div class=\"sc-step-number\">Step %02d</div>\n" (i + 1));
      if step.title <> "" then
        Buffer.add_string buf
          (Printf.sprintf "      <h2>%s</h2>\n" (html_escape step.title));
      if step.prose <> "" then
        Buffer.add_string buf
          (Printf.sprintf "      <p>%s</p>\n" (html_escape step.prose));
      (* Hidden code slot for JS to read *)
      Buffer.add_string buf "      <div class=\"sc-code-slot\">\n";
      Buffer.add_string buf (generate_code_lines step.code step.focus);
      Buffer.add_string buf "      </div>\n";
      Buffer.add_string buf "    </div>\n")
    steps;
  Buffer.add_string buf "  </div>\n";

  (* Code column *)
  Buffer.add_string buf "  <div class=\"sc-code-col\">\n";
  Buffer.add_string buf "    <div class=\"sc-code-panel\">\n";
  Buffer.add_string buf "      <div class=\"sc-code-header\">\n";
  Buffer.add_string buf
    "        <div class=\"sc-dots\"><span></span><span></span><span></span></div>\n";
  Buffer.add_string buf
    (Printf.sprintf "        <span class=\"sc-filename\">%s</span>\n"
       (html_escape filename));
  Buffer.add_string buf
    (Printf.sprintf
       "        <span class=\"sc-step-badge\">1 / %d</span>\n"
       (List.length steps));
  Buffer.add_string buf "      </div>\n";
  Buffer.add_string buf "      <div class=\"sc-code-body\">\n";
  (* Initial code from first step *)
  (match steps with
  | first :: _ -> Buffer.add_string buf (generate_code_lines first.code first.focus)
  | [] -> ());
  Buffer.add_string buf "      </div>\n";
  Buffer.add_string buf "    </div>\n";
  Buffer.add_string buf "  </div>\n";

  Buffer.add_string buf "</div>\n";

  (* JavaScript *)
  Buffer.add_string buf "<script>\n";
  Buffer.add_string buf shared_js;
  Buffer.add_string buf "</script>\n";

  Buffer.contents buf

(** {1 Extension Registration} *)

module Scrolly : Odoc_extension_api.Extension = struct
  let prefix = "scrolly"

  let to_document ~tag content =
    (* Extract theme from tag: scrolly.warm, scrolly.dark, scrolly.notebook *)
    let theme =
      match String.index_opt tag '.' with
      | None -> "warm"
      | Some i -> String.sub tag (i + 1) (String.length tag - i - 1)
    in
    let tutorial_title, steps = extract_steps content in
    let filename =
      match theme with
      | "dark" -> "main.ml"
      | "notebook" -> "test.ml"
      | _ -> "parser.ml"
    in
    let html = generate_html ~theme ~title:tutorial_title ~filename steps in
    let block : Block.t =
      [
        {
          Odoc_document.Types.Block.attr = [ "scrollycode" ];
          desc = Raw_markup ("html", html);
        };
      ]
    in
    { Odoc_extension_api.content = block; overrides = []; resources = []; assets = [] }
end

let () = Odoc_extension_api.Registry.register (module Scrolly)
