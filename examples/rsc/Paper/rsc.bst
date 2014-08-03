%%
%% This is file `rsc.bst',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% rsc.dtx  (with options: `bst,rsc')
%% ----------------------------------------------------------------
%% rsc --- BibTeX styles for Royal Society of Chemistry and Wiley
%%   journals
%% E-mail: joseph.wright@morningstar2.co.uk
%% Released under the LaTeX Project Public License v1.3c or later
%% See http://www.latex-project.org/lppl.txt
%% ----------------------------------------------------------------
%% 
ENTRY
  { address
    author
    booktitle
    chapter
    ctrl-use-title
    ctrl-etal-number
    ctrl-use-doi-all
    doi
    edition
    editor
    howpublished
    institution
    journal
    key
    note
    number
    organization
    pages
    publisher
    school
    series
    title
    type
    url
    volume
    year
  }
  {}
  { label
    extra.label
    short.list
  }

INTEGERS { output.state before.all mid.sentence }
INTEGERS { after.sentence after.block after.item }
INTEGERS { author.or.editor }

FUNCTION {init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
  #4 'after.item :=
}

INTEGERS { is.use.title etal.number use.doi.all }

%% #0 turns off the display of the title for articles
%% #1 enables
FUNCTION {default.is.use.title} { #0 }

%% The number of names that force "et al." to be used
FUNCTION {default.etal.number} { #0 }

%% #0 turns off the display of the DOI for articles
%% #1 enables
FUNCTION {default.use.doi.all} { #0 }

FUNCTION {add.comma}
{ ", " * }

STRINGS { s t }

FUNCTION {output.nonnull}
{ 's :=
  output.state mid.sentence =
    { add.comma write$ }
    { output.state after.block =
      { add.comma write$
        newline$
        "\newblock " write$
      }
      { output.state before.all =
          'write$
          { output.state after.item =
            { " " * write$ }
            { add.period$ " " * write$ }
          if$
          }
        if$
        }
      if$
      mid.sentence 'output.state :=
    }
  if$
  s
}

FUNCTION {output}
{ duplicate$ empty$
    'pop$
    'output.nonnull
  if$
}

FUNCTION {output.check}
{ 't :=
  duplicate$ empty$
    { pop$ "Empty " t * " in " * cite$ * warning$ }
    'output.nonnull
  if$
}

INTEGERS { would.add.period.textlen }

FUNCTION {would.add.period}
{ duplicate$
  add.period$
  text.length$
  'would.add.period.textlen :=
  duplicate$
  text.length$
  would.add.period.textlen =
    { #0 }
    { #1 }
  if$
}

FUNCTION {fin.entry}
{ would.add.period
    { "\relax" * write$ newline$
      "\mciteBstWouldAddEndPuncttrue" write$ newline$
      "\mciteSetBstMidEndSepPunct{\mcitedefaultmidpunct}"
      write$ newline$
      "{\mcitedefaultendpunct}{\mcitedefaultseppunct}\relax"
    }
    { "\relax" * write$ newline$
      "\mciteBstWouldAddEndPunctfalse" write$ newline$
      "\mciteSetBstMidEndSepPunct{\mcitedefaultmidpunct}"
      write$ newline$
      "{}{\mcitedefaultseppunct}\relax"
    }
  if$
  write$
  newline$
  "\EndOfBibitem" write$
}

FUNCTION {not}
{   { #0 }
    { #1 }
  if$
}

FUNCTION {and}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION {or}
{   { pop$ #1 }
    'skip$
  if$
}

FUNCTION {field.or.null}
{ duplicate$ empty$
    { pop$ "" }
    'skip$
  if$
}

FUNCTION {emphasize}
{ duplicate$ empty$
    { pop$ "" }
    { "\emph{" swap$ * "}" * }
  if$
}

FUNCTION {boldface}
{ duplicate$ empty$
    { pop$ "" }
    { "\textbf{" swap$ * "}" * }
  if$
}

FUNCTION {paren}
{ duplicate$ empty$
    { pop$ "" }
    { "(" swap$ * ")" * }
  if$
}

FUNCTION {bbl.and}
{ "and" }

FUNCTION {bbl.chapter}
{ "ch." }

FUNCTION {bbl.doi}
{ "DOI:" }

FUNCTION {bbl.editor}
{ "ed." }


FUNCTION {bbl.edition}
{ "edn" }

FUNCTION {bbl.etal}
{ "et~al." emphasize }

FUNCTION {bbl.in}
{ "in" }

FUNCTION {bbl.inpress}
{ "in press" }

FUNCTION {bbl.msc}
{ "MSc thesis" }

FUNCTION {bbl.page}
{ "p." }

FUNCTION {bbl.pages}
{ "pp." }

FUNCTION {bbl.phd}
{ "PhD thesis" }

FUNCTION {bbl.submitted}
{ "submitted for publication" }

FUNCTION {bbl.techreport}
{ "Technical Report" }

FUNCTION {bbl.volume}
{ "vol." }

FUNCTION {bbl.first}
{ "1st" }

FUNCTION {bbl.second}
{ "2nd" }

FUNCTION {bbl.third}
{ "3rd" }

FUNCTION {bbl.fourth}
{ "4th" }

FUNCTION {bbl.fifth}
{ "5th" }

FUNCTION {bbl.st}
{ "st" }

FUNCTION {bbl.nd}
{ "nd" }

FUNCTION {bbl.rd}
{ "rd" }

FUNCTION {bbl.th}
{ "th" }

FUNCTION {eng.ord}
{ duplicate$ "1" swap$ *
  #-2 #1 substring$ "1" =
     { bbl.th * }
     { duplicate$ #-1 #1 substring$
       duplicate$ "1" =
         { pop$ bbl.st * }
         { duplicate$ "2" =
             { pop$ bbl.nd * }
             { "3" =
                 { bbl.rd * }
                 { bbl.th * }
               if$
             }
           if$
          }
       if$
     }
   if$
}

INTEGERS{ l }

FUNCTION{string.length}
{ #1 'l :=
  { duplicate$ duplicate$ #1 l substring$ = not }
    { l #1 + 'l := }
  while$
  pop$ l
}

STRINGS{replace find text}

INTEGERS{find_length}

FUNCTION{find.replace}
{ 'replace :=
  'find :=
  'text :=
  find  string.length 'find_length :=
  ""
  { text empty$ not }
    { text #1 find_length substring$ find =
      { replace *
        text #1 find_length + global.max$ substring$ 'text :=
      }
      { text #1 #1 substring$ *
        text #2 global.max$ substring$ 'text :=
      }
      if$
    }
  while$
}

FUNCTION {chr.to.value}
{ chr.to.int$ #48 -
  duplicate$ duplicate$
  #0 < swap$ #9 > or
    { #48 + int.to.chr$
      " is not a number..." *
      warning$
     pop$ #0
    }
    {}
  if$
}

FUNCTION{is.a.digit}
{ duplicate$ "" =
    {pop$ #0}
    {chr.to.int$ #48 - duplicate$
     #0 < swap$ #9 > or not}
  if$
}

FUNCTION{is.a.number}
{
  { duplicate$ #1 #1 substring$ is.a.digit }
    {#2 global.max$ substring$}
  while$
  "" =
}

FUNCTION {extract.num}
{ duplicate$ 't :=
  "" 's :=
  { t empty$ not }
  { t #1 #1 substring$
    t #2 global.max$ substring$ 't :=
    duplicate$ is.a.number
      { s swap$ * 's := }
      { pop$ "" 't := }
    if$
  }
  while$
  s empty$
    'skip$
    { pop$ s }
  if$
}

FUNCTION {bibinfo.check}
{ swap$
  duplicate$ missing$
    { pop$ pop$
      ""
    }
    { duplicate$ empty$
        {
          swap$ pop$
        }
        { swap$
          pop$
        }
      if$
    }
  if$
}

FUNCTION {convert.edition}
{ extract.num "l" change.case$ 's :=
  s "first" = s "1" = or
    { bbl.first 't := }
    { s "second" = s "2" = or
        { bbl.second 't := }
        { s "third" = s "3" = or
            { bbl.third 't := }
            { s "fourth" = s "4" = or
                { bbl.fourth 't := }
                { s "fifth" = s "5" = or
                    { bbl.fifth 't := }
                    { s #1 #1 substring$ is.a.number
                        { s eng.ord 't := }
                        { edition 't := }
                      if$
                    }
                  if$
                }
              if$
            }
          if$
        }
      if$
    }
  if$
  t
}

FUNCTION {tie.or.space.connect}
{ duplicate$ text.length$ #3 <
    { "~" }
    { " " }
  if$
  swap$ * *
}

FUNCTION {space.connect}
{ " " swap$ * * }

INTEGERS { nameptr namesleft numnames }

FUNCTION {format.names}
{ 's :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
  numnames etal.number > etal.number #0 > and
    { s #1 "{f.~}{vv~}{ll}{, jj}" format.name$ 't :=
      t bbl.etal space.connect
    }
    {
      { namesleft #0 > }
        { s nameptr "{f.~}{vv~}{ll}{, jj}" format.name$ 't :=
          nameptr #1 >
            { namesleft #1 >
              { add.comma t * }
              { numnames #2 >
                { "" * }
                'skip$
              if$
              t "others" =
                { bbl.etal space.connect }
                { bbl.and space.connect t space.connect }
              if$
              }
            if$
            }
          't
        if$
        nameptr #1 + 'nameptr :=
        namesleft #1 - 'namesleft :=
        }
    while$
  }
  if$
}

FUNCTION {format.authors}
{ author empty$
    { "" }
    { #1 'author.or.editor :=
      author format.names }
  if$
}

FUNCTION {strip.comma}
{ duplicate$
  string.length 'find_length :=
  duplicate$
  find_length #1 - #1 substring$
  "," =
    { #1 find_length #2 - substring$
      " " *
    }
    'skip$
  if$
}

FUNCTION {format.editors}
{ editor empty$
    { "" }
    { #2 'author.or.editor :=
      bbl.editor " " * *
      editor format.names
    }
  if$
}

FUNCTION {format.doi}
{ use.doi.all
    { doi empty$
        { bbl.doi doi tie.or.space.connect output }
        'skip$
      if$
    }
    'skip$
  if$
}

FUNCTION {n.separate.multi}
{ 't :=
  ""
  #0 'numnames :=
  t text.length$ #4 > t is.a.number and
    {
      { t empty$ not }
      { t #-1 #1 substring$ is.a.number
          { numnames #1 + 'numnames := }
          { #0 'numnames := }
        if$
        t #-1 #1 substring$ swap$ *
        t #-2 global.max$ substring$ 't :=
        numnames #4 =
          { duplicate$ #1 #1 substring$ swap$
            #2 global.max$ substring$
            "\," swap$ * *
            #1 'numnames :=
          }
          'skip$
        if$
      }
      while$
    }
    { t swap$ * }
  if$
}

FUNCTION {format.bvolume}
{ volume empty$
    { "" }
    { bbl.volume volume tie.or.space.connect }
  if$
}

FUNCTION {format.title.noemph}
{ 't :=
  t empty$
    { "" }
    { t }
  if$
}

FUNCTION {format.title}
{ 't :=
  t empty$
    { "" }
    { t emphasize }
  if$
}

FUNCTION {format.url}
{ url empty$
    { "" }
    { "\url{" url * "}" * }
  if$
}

FUNCTION {format.title.vol}
{ 't :=
  t empty$
    { "" }
    { t emphasize }
  if$
  volume empty$
    'skip$
    { format.bvolume emphasize
      swap$ add.comma swap$ *
    }
  if$
  }

FUNCTION {format.full.names}
{'s :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr
      "{vv~}{ll}" format.name$ 't :=
      nameptr #1 >
        {
          namesleft #1 >
            { ", " * t * }
            {
              numnames #2 >
                { "," * }
                'skip$
              if$
              t "others" =
                { bbl.etal * }
                { bbl.and space.connect t space.connect }
              if$
            }
          if$
        }
        't
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {author.editor.full}
{ author empty$
    { editor empty$
        { "" }
        { editor format.full.names }
      if$
    }
    { author format.full.names }
  if$
}

FUNCTION {author.full}
{ author empty$
    { "" }
    { author format.full.names }
  if$
}

FUNCTION {editor.full}
{ editor empty$
    { "" }
    { editor format.full.names }
  if$
}

FUNCTION {make.full.names}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.full
    { type$ "proceedings" =
        'editor.full
        'author.full
      if$
    }
  if$
}


FUNCTION {output.bibitem} { newline$
  "\bibitem[" write$
  label write$
  ")" make.full.names duplicate$ short.list =
     { pop$ }
     { * }
   if$
  "]{" * write$
  cite$ write$
  "}" write$
  newline$
  ""
  before.all 'output.state :=
}

FUNCTION {n.dashify} { 't :=
  ""
    { t empty$ not }
    { t #1 #1 substring$ "-" =
    { t #1 #2 substring$ "--" = not
        { "--" *
          t #2 global.max$ substring$ 't :=
        }
        {   { t #1 #1 substring$ "-" = }
        { "-" *
          t #2 global.max$ substring$ 't :=
        }
          while$
        }
      if$
    }
    { t #1 #1 substring$ *
      t #2 global.max$ substring$ 't :=
    }
      if$
    }
  while$
}

FUNCTION {format.date}
{ year empty$
    { "" }
    'year
  if$
}

FUNCTION {format.bdate}
{ year empty$
    { "There's no year in " cite$ * warning$ }
    'year
  if$
}

FUNCTION {either.or.check}
{ empty$
    'pop$
    { "Can't use both " swap$ * " fields in "
      * cite$ * warning$ }
  if$
}

FUNCTION {format.edition}
{ edition duplicate$ empty$
    'skip$
    { convert.edition
      bbl.edition bibinfo.check
      " " * bbl.edition *
    }
  if$
}

INTEGERS { multiresult }

FUNCTION {multi.page.check}
{ 't :=
  #0 'multiresult :=
    { multiresult not
      t empty$ not
      and
    }
    { t #1 #1 substring$
      duplicate$ "-" =
      swap$ duplicate$ "," =
      swap$ "+" =
      or or
        { #1 'multiresult := }
        { t #2 global.max$ substring$ 't := }
      if$
    }
  while$
  multiresult
}

FUNCTION {format.pages}
{ pages empty$
    { "" }
    { pages multi.page.check
      { bbl.pages pages n.dashify tie.or.space.connect }
      { bbl.page pages tie.or.space.connect }
    if$
    }
  if$
}

FUNCTION {format.pages.required}
{ pages empty$
    { ""
      "There are no page numbers for " cite$ * warning$
      output
    }
    { pages multi.page.check
      { bbl.pages pages n.dashify tie.or.space.connect }
      { bbl.page pages tie.or.space.connect }
    if$
    }
  if$
}

FUNCTION {format.pages.nopp}
{ pages empty$
    { ""
      "There are no page numbers for " cite$ * warning$
      output
    }
    { pages multi.page.check
      { pages n.dashify space.connect }
      { pages space.connect }
    if$
    }
  if$
}

FUNCTION {format.pages.patent}
{ pages empty$
    { "There is no patent number for " cite$ * warning$ }
    { pages multi.page.check
      { pages n.dashify }
      { pages n.separate.multi }
      if$
    }
  if$
}

FUNCTION {format.vol.pages}
{ volume boldface field.or.null
  duplicate$ empty$
    { pop$ format.pages.required }
    { add.comma pages n.dashify * }
  if$
}

FUNCTION {format.chapter.pages}
{ chapter empty$
    'format.pages
    { type empty$
    { bbl.chapter }
    { type "l" change.case$ }
      if$
      chapter tie.or.space.connect
      pages empty$
    'skip$
    { add.comma format.pages * }
      if$
    }
  if$
}

FUNCTION {format.title.in}
{ 's :=
  s empty$
    { "" }
    { editor empty$
      { bbl.in s format.title space.connect }
      { bbl.in s format.title space.connect
      add.comma format.editors *
      }
    if$
    }
  if$
}

FUNCTION {format.title.vol.in}
{ 's :=
  after.item 'output.state :=
  s empty$
    { "" }
    { editor empty$
      { bbl.in s format.title.vol space.connect }
      { bbl.in s format.title.vol space.connect
      add.comma format.editors *
      }
    if$
    }
  if$
}

FUNCTION {format.pub.address}
{ publisher empty$
    { "" }
    { address empty$
        { publisher }
        { publisher add.comma address *}
      if$
    }
  if$
}

FUNCTION {empty.misc.check}
{ author empty$ title empty$ howpublished empty$
  year empty$ note empty$ url empty$
  and and and and and
    { "all relevant fields are empty in " cite$ * warning$ }
    'skip$
  if$
}

FUNCTION {empty.doi.note}
{ doi empty$ note empty$ and
    { "Need either a note or DOI for " cite$ * warning$ }
    'skip$
  if$
}

FUNCTION {format.thesis.type}
{ type empty$
    'skip$
    { pop$
      type emphasize
    }
  if$
}

FUNCTION {article}
{ output.bibitem
  format.authors "author" output.check
  is.use.title
    { title format.title.noemph "title" output.check }
    'skip$
  if$
  journal emphasize "journal" output.check
  format.date "year" output.check
  volume empty$
    { "" format.pages.nopp output }
    { format.vol.pages output }
  if$
  format.doi
  fin.entry
}

FUNCTION {book}
{ output.bibitem
  author empty$
    { title format.title "title" output.check
      editor empty$
        { "Need either an author or editor for "
          cite$ * warning$ }
        { "" format.editors * "editor" output.check }
      if$
    }
    { format.authors output
      "author and editor" editor either.or.check
      title format.title "title" output.check
    }
  if$
  format.pub.address "publisher" output.check
  format.edition output
  format.bdate "year" output.check
  format.bvolume output
  pages empty$
    'skip$
    { format.pages output }
  if$
  format.doi
  fin.entry
}

FUNCTION {booklet}
{ output.bibitem
  format.authors output
  title format.title "title" output.check
  howpublished output
  address output
  format.date output
  format.doi
  fin.entry
}

FUNCTION {inbook}
{ output.bibitem
  author empty$
    { title format.title "title" output.check
      editor empty$
      { "Need at least an author or an editor for "
        cite$ * warning$ }
      { "" format.editors * "editor" output.check }
    if$
    }
    { format.authors output
      title format.title.in "title" output.check
    }
  if$
  format.pub.address "publisher" output.check
  format.edition output
  format.bdate "year" output.check
  format.bvolume output
  format.chapter.pages "chapter and pages" output.check
  format.doi
  fin.entry
}

FUNCTION {incollection}
{ output.bibitem
  author empty$
    { booktitle
      format.title
      "booktitle" output.check
      editor empty$
        { "Need at least an author or an editor for "
          cite$ * warning$ }
        { "" format.editors * "editor" output.check }
      if$
    }
    { format.authors output
      booktitle
      format.title
      "booktitle" output.check
    }
  if$
  format.pub.address "publisher" output.check
  format.edition output
  format.bdate "year" output.check
  format.bvolume output
  format.chapter.pages "chapter and pages" output.check
  format.doi
  fin.entry
}

FUNCTION {inpress}
{ output.bibitem
  format.authors "author" output.check
  journal emphasize "journal" output.check
  bbl.inpress output
  format.doi
  fin.entry
}

FUNCTION {inproceedings}
{ output.bibitem
  format.authors "author" output.check
  booktitle format.title.noemph "booktitle" output.check
  address output
  format.date "year" output.check
  pages empty$
    'skip$
    { format.pages output }
  if$
  format.doi
  fin.entry
}

FUNCTION {manual}
{ output.bibitem
  author empty$
    { organization empty$
      'skip$
      { organization output
        address output
      }
      if$
    }
    { format.authors output }
  if$
  title format.title "title" output.check
  author empty$
    { organization empty$
      { address output }
      'skip$
    if$
    }
    { organization output
      address output
    }
  if$
  format.edition output
  format.date output
  format.doi
  fin.entry
}

FUNCTION {mastersthesis}
{ output.bibitem
  format.authors "author" output.check
  bbl.msc emphasize format.thesis.type output
  school "school" output.check
  address output
  format.date "year" output.check
  format.doi
  fin.entry
}

FUNCTION {misc}
{ output.bibitem
  format.authors output
  title empty$
    'skip$
    { title format.title output }
  if$
  howpublished output
  year output
  format.url output
  note output
  format.doi
  fin.entry
  empty.misc.check
}

FUNCTION {patent}
{ output.bibitem
  organization empty$
    { format.authors "author and organization" output.check }
    { author empty$
        { organization }
        {  format.authors organization paren space.connect }
      if$
      "author and organization" output.check
    }
  if$
  journal emphasize "journal" output.check
  format.pages.patent "pages" output.check
  format.date "year" output.check
  format.doi
  fin.entry
}

FUNCTION {phdthesis}
{ output.bibitem
  format.authors "author" output.check
  bbl.phd emphasize format.thesis.type output
  school "school" output.check
  address output
  format.date "year" output.check
  format.doi
  fin.entry
}

FUNCTION {proceedings}
{ output.bibitem
  title format.title.noemph "title" output.check
  address output
  format.date "year" output.check
  pages empty$
    'skip$
    { format.pages output }
  if$
  format.doi
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  format.authors "author" output.check
  title format.title "title" output.check
  institution
  type empty$
    'bbl.techreport
    'type
  if$
  space.connect
  number empty$
    { "t" change.case$ }
    { number tie.or.space.connect }
  if$
  output
  format.pub.address output
  format.date "year" output.check
  format.doi
  fin.entry
}

FUNCTION {unpublished}
{ output.bibitem
  format.authors "author" output.check
  journal empty$
    'skip$
    { journal emphasize "journal" output.check }
  if$
  doi empty$
    {  note output }
    {
       format.date output
       bbl.doi doi tie.or.space.connect output
   }
  if$
  fin.entry
  empty.doi.note
}

INTEGERS { a b }

FUNCTION {mult}
{ 'a :=
  'b :=
  b #0 <
    {#-1 #0 b - 'b :=}
    {#1}
  if$
  #0
  {b #0 >}
    { a +
      b #1 - 'b :=
    }
  while$
  swap$
    'skip$
    {#0 swap$ -}
    if$
}

FUNCTION {str.to.int.aux}
{ {duplicate$ empty$ not}
    { swap$ #10 mult 'a :=
      duplicate$ #1 #1 substring$
      chr.to.value a +
      swap$
     #2 global.max$ substring$
    }
  while$
  pop$
}

FUNCTION {str.to.int}
{ duplicate$ #1 #1 substring$ "-" =
    {#1 swap$ #2 global.max$ substring$}
    {#0 swap$}
  if$
  #0 swap$ str.to.int.aux
  swap$
    {#0 swap$ -}
    {}
  if$
}

FUNCTION {yes.no.to.int}
{ "l" change.case$ duplicate$
    "yes" =
    { pop$  #1 }
    { duplicate$ "no" =
        { pop$ #0 }
        { "unknown Boolean " quote$ * swap$ * quote$ *
          " in " * cite$ * warning$
          #0
        }
      if$
    }
  if$
}

FUNCTION {Control}
{ ctrl-use-title
  empty$
    { skip$ }
    { ctrl-use-title
      yes.no.to.int
      'is.use.title := }
  if$
  ctrl-etal-number
  empty$
    { skip$ }
    { ctrl-etal-number
      str.to.int
      'etal.number := }
  if$
  ctrl-use-doi-all
  empty$
    { skip$ }
    { ctrl-use-doi-all
      yes.no.to.int
      'use.doi.all := }
  if$
}

FUNCTION {conference} {inproceedings}

FUNCTION {other} {patent}

FUNCTION {default.type} {misc}

MACRO {jan} {"January"}
MACRO {feb} {"February"}
MACRO {mar} {"March"}
MACRO {apr} {"April"}
MACRO {may} {"May"}
MACRO {jun} {"June"}
MACRO {jul} {"July"}
MACRO {aug} {"August"}
MACRO {sep} {"September"}
MACRO {oct} {"October"}
MACRO {nov} {"November"}
MACRO {dec} {"December"}

READ

FUNCTION {initialize.controls}
{ default.is.use.title 'is.use.title :=
  default.etal.number 'etal.number :=
  default.use.doi.all 'use.doi.all :=
}

INTEGERS { len }

FUNCTION {chop.word}
{ 's :=
  'len :=
  s #1 len substring$ =
    { s len #1 + global.max$ substring$ }
    's
  if$
}

FUNCTION {format.lab.names}
{ 's :=
  s #1 "{vv~}{ll}" format.name$
  s num.names$ duplicate$
  #2 >
    { pop$ bbl.etal space.connect }
    { #2 <
        'skip$
        { s #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            { bbl.etal space.connect }
            { bbl.and space.connect s #2 "{vv~}{ll}"
              format.name$ space.connect }
          if$
        }
      if$
    }
  if$
}

FUNCTION {author.key.label}
{ author empty$
    { key empty$
        { cite$ #1 #3 substring$ }
        'key
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {author.editor.key.label}
{ author empty$
    { editor empty$
        { key empty$
            { cite$ #1 #3 substring$ }
            'key
          if$
        }
        { editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {author.key.organization.label}
{ author empty$
    { key empty$
        { organization empty$
            { cite$ #1 #3 substring$ }
            { "The " #4 organization chop.word #3
              text.prefix$ }
          if$
        }
        'key
      if$
    }
    { author format.lab.names }
  if$
}

FUNCTION {editor.key.organization.label}
{ editor empty$
    { key empty$
        { organization empty$
            { cite$ #1 #3 substring$ }
            { "The " #4 organization chop.word #3
              text.prefix$ }
          if$
        }
        'key
      if$
    }
    { editor format.lab.names }
  if$
}

FUNCTION {calc.short.authors}
{ type$ "book" =
  type$ "inbook" =
  or
    'author.editor.key.label
    { type$ "proceedings" =
        'editor.key.organization.label
        { type$ "manual" =
            'author.key.organization.label
            'author.key.label
          if$
        }
      if$
    }
  if$
  'short.list :=
}

FUNCTION {calc.label}
{ calc.short.authors
  short.list
  "("
  *
  year duplicate$ empty$
  short.list key field.or.null = or
     { pop$ "" }
     'skip$
  if$
  *
  'label :=
}

ITERATE {calc.label}

STRINGS { longest.label last.label next.extra }

INTEGERS { longest.label.width last.extra.num number.label }

FUNCTION {initialize.longest.label}
{ "" 'longest.label :=
  #0 int.to.chr$ 'last.label :=
  "" 'next.extra :=
  #0 'longest.label.width :=
  #0 'last.extra.num :=
  #0 'number.label :=
}

FUNCTION {forward.pass}
{ last.label label =
    { last.extra.num #1 + 'last.extra.num :=
      last.extra.num int.to.chr$ 'extra.label :=
    }
    { "a" chr.to.int$ 'last.extra.num :=
      "" 'extra.label :=
      label 'last.label :=
    }
  if$
  number.label #1 + 'number.label :=
}

EXECUTE {initialize.longest.label}

ITERATE {forward.pass}

FUNCTION {begin.bib}
{ preamble$ empty$
    'skip$
    { preamble$ write$ newline$ }
  if$
  "\providecommand*{\mcitethebibliography}{\thebibliography}"
  write$ newline$
  "\csname @ifundefined\endcsname{endmcitethebibliography}"
  write$ newline$
  "{\let\endmcitethebibliography\endthebibliography}{}"
  write$ newline$
  "\begin{mcitethebibliography}{"  number.label int.to.str$  * "}" *
  write$ newline$
  "\providecommand*{\natexlab}[1]{#1}"
  write$ newline$
  "\providecommand*{\mciteSetBstSublistMode}[1]{}"
  write$ newline$
  "\providecommand*{\mciteSetBstMaxWidthForm}[2]{}"
  write$ newline$
  "\providecommand*{\mciteBstWouldAddEndPuncttrue}"
  write$ newline$
  "  {\def\EndOfBibitem{\unskip.}}"
  write$ newline$
  "\providecommand*{\mciteBstWouldAddEndPunctfalse}"
  write$ newline$
  "  {\let\EndOfBibitem\relax}"
  write$ newline$
  "\providecommand*{\mciteSetBstMidEndSepPunct}[3]{}"
  write$ newline$
  "\providecommand*{\mciteSetBstSublistLabelBeginEnd}[3]{}"
  write$ newline$
  "\providecommand*{\EndOfBibitem}{}"
  write$ newline$
  "\mciteSetBstSublistMode{f}"
  write$ newline$
  "\mciteSetBstMaxWidthForm{subitem}"
  write$ newline$
  "{(\emph{\alph{mcitesubitemcount}})}"
  write$ newline$
  "\mciteSetBstSublistLabelBeginEnd{\mcitemaxwidthsubitemform\space}"
  write$ newline$
  "{\relax}{\relax}"
  write$ newline$
}

EXECUTE {begin.bib}

EXECUTE {init.state.consts}

ITERATE {call.type$}

FUNCTION {end.bib}
{ newline$
  "\end{mcitethebibliography}" write$ newline$
}

EXECUTE {end.bib}
%% 
%% Copyright (C) 2006-2009 by
%%   Joseph Wright <joseph.wright@morningstar2.co.uk>
%% 
%% It may be distributed and/or modified under the conditions of
%% the LaTeX Project Public License (LPPL), either version 1.3c of
%% this license or (at your option) any later version.  The latest
%% version of this license is in the file:
%% 
%%    http://www.latex-project.org/lppl.txt
%% 
%% This work is "maintained" (as per LPPL maintenance status) by
%%   Joseph Wright.
%% 
%% This work consists of the file  rsc.dtx
%%           and the derived files rsc.pdf,
%%                                 rsc.ins,
%%                                 rsc.sty,
%%                                 rsc.bib and
%%                                 rsc-demo.tex.
%% 
%% 
%%
%% End of file `rsc.bst'.
