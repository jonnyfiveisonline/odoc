OxCaml mode, jkind, and modality rendering tests.

Compile the test .mli with OxCaml and run through the odoc pipeline:

  $ ocamlc -bin-annot -c test_modes.mli
  $ odoc compile --package test test_modes.cmti
  $ odoc link test_modes.odoc
  $ odoc html-generate test_modes.odocl -o html --indent

Check arrow argument modes (@ local, @ unique):

  $ grep 'keyword.*@.*local' html/test/Test_modes/index.html | head -1 | sed 's/ *$//'
         <span>string <span class="keyword">@</span> local

  $ grep 'keyword.*@.*unique' html/test/Test_modes/index.html | head -1 | sed 's/ *$//'
         <span>string <span class="keyword">@</span> unique

Multiple argument modes on one arrow:

  $ grep 'keyword.*@.*local unique' html/test/Test_modes/index.html | head -1 | sed 's/ *$//'
         <span>string <span class="keyword">@</span> local unique

Arrow return modes (@ after arrow):

  $ grep 'keyword.*@.*local' html/test/Test_modes/index.html | grep -v 'string' | sed 's/ *$//'
         <span class="keyword">@</span> local
         </span> int <span class="keyword">@</span> local

Value modalities with @@ syntax:

  $ grep 'keyword.*@@' html/test/Test_modes/index.html | sed 's/ *$//'
         <span class="keyword">@@</span> portable
         <span class="keyword">@@</span> global

Normal function has no @@ or @ mode annotations:

  $ grep 'val-normal_fun' html/test/Test_modes/index.html | head -1
      <div class="spec value anchored" id="val-normal_fun">
  $ grep -c 'keyword.*@' html/test/Test_modes/index.html
  8

Type parameter jkinds:

  $ grep 'float64' html/test/Test_modes/index.html
         <span>('a : float64) float_box</span>
  $ grep 'immediate' html/test/Test_modes/index.html
         <span>('a : immediate) imm_box</span>
