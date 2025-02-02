Digitised comparative word list of Malay, Nias, Toba-Batak, and Enggano
in Modigliani’s “L’isola Delle Donne” from 1894
================
[Gede Primahadi Wijaya
Rajeg](https://www.ling-phil.ox.ac.uk/people/gede-rajeg)
<a itemprop="sameAs" content="https://orcid.org/0000-0002-2047-8621" href="https://orcid.org/0000-0002-2047-8621" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a></br>University
of Oxford/Udayana University

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-oxweb-logo.gif"
width="84" alt="The University of Oxford" />](https://www.ox.ac.uk/)
[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-lingphil.png"
width="83"
alt="Faculty of Linguistics, Philology and Phonetics, the University of Oxford" />](https://www.ling-phil.ox.ac.uk/)
[<img
src="https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/file-ahrc.png"
width="325" alt="Arts and Humanities Research Council (AHRC)" />](https://www.ukri.org/councils/ahrc/)
</br>*This work is funded by the Arts and Humanities Research Council
(AHRC) (Grant ID:
[AH/W007290/1](https://gtr.ukri.org/projects?ref=AH%2FW007290%2F1),
“**Lexical resources for Enggano, a threatened language of
Indonesia**”), led by the Faculty of Linguistics, Philology and
Phonetics at the University of Oxford, UK. Visit the [central webpage of
the Enggano project](https://enggano.ling-phil.ox.ac.uk/)*.

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/">

<span property="dct:title">Digitised, annotated comparative word list in
Modigliani’s “L’isola delle donne” from 1894</span> by
<a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://www.ling-phil.ox.ac.uk/people/gede-rajeg">Gede
Primahadi W. Rajeg</a> is licensed under
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">Creative
Commons Attribution-NonCommercial-ShareAlike 4.0
International<img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/><img src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1" style="height:22px!important;margin-left:3px;vertical-align:text-bottom;"/></a>

</p>
<!-- badges: end -->

## Overview

The
[data-source](https://github.com/engganolang/modigliani-1894/tree/main/data-source)
directory contains the original data in .xlsx file that the first author
hand-digitised from the original source (Modigliani 1894). The light
annotation included reflects the content of the original source,
covering several aspects. First, annotating the string component that is
printed in italics in the original source; the marking is indicated by
the XML tag `<i>` so it can be traced computationally. Second, there is
also annotation concerning remark (`<rm...>`) for a given language
column in the original source, and that concerning aspect of meaning
(`<sem...>`). These annotations are still available in the `WORD` column
of the
[data-output](https://github.com/engganolang/modigliani-1894/tree/main/data-output)
with
[long-table](https://github.com/engganolang/modigliani-1894/blob/main/data-output/modigliani-1894-long-table.csv)
format (the column `WORD2` excludes these annotations, which are
transferred into the `REMARK` column of the long-table format). In the
[wide-table](https://github.com/engganolang/modigliani-1894/blob/main/data-output/modigliani-1894-wide-table.csv)
format of the data-output, the language columns named with `..._1` do
not contain these annotations, which have been transferred into other
columns named with `..._rm` and `..._sem` labels. The `REMARK` column in
the two sets of data-output contains another annotation I put while
transcribing from the source: the cell beginning with `M--` is a comment
for the Malay column while that starting with `I--` is for the Italian
(the reference language).

Information concerning the orthography standardisation is available on
the README page of the [ortho
directory](https://github.com/engganolang/modigliani-1894/tree/main/ortho).

### References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-modigliani1894" class="csl-entry">

Modigliani, Elio. 1894. *L’isola Delle Donne*. Milano: Ulrico Hoepli.
<https://www.google.co.uk/books/edition/L_isola_delle_donne/gksCAAAAMAAJ?hl=en&gbpv=0>.

</div>

</div>
