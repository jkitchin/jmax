# bibtex-utils.el #

To use this package, add the following line to your .emacs:

```
  (require 'bibtex-utils)
```

## Keywords

`bibtex-utils` adds a hook to `bibtex-mode-hook`. The hook will run whenever a `.bib` files is opened, parsing all the keywords present in the file and storing them in a local variable. Those keywords will be used to provide tab-completion when the user calls the function `bu-make-field-keywords`.

When point is within a bibtex entry, calling `bu-make-field-keywords` will prompt the user for keywords, with tab-completion. If a keywords field already exists, the keywords will be added to it. If not, a new field will be added to the entry. Hitting return on its own will complete the process. Any keywords not already in the local variable are added to it.

You will probably want to bind this function to a key:

```
  (eval-after-load 'bibtex
    '(define-key bibtex-mode-map "\C-ck" 'bu-make-field-keywords))
```

These functions assume that keywords consist of phrases with no commas; commas are interpreted as separators between keywords. Newlines are ignored, so keywords can span multiple lines.

## Viewing documents

`bibtex-utils` also provides a mechanism to open the documents (pdfs, webpages) for a given bibtex entry. This is currently hard-coded to suit my personal directory structure, but I will be generalizing the system shortly to make it more customizable.

### pdfs

For the moment, this system assumes that all of your pdfs are in a single directory, saved in the customizable variable `bu-pdf-dir`.  By default this is set to `~/pdfs`. You can also specify the path to your pdf viewer in the variable `bu-pdf-viewer`, which by default is `/usr/bin/okular`. For now, the files must be named the same as the entry key, with the '.pdf' suffix added. This will become customizable shortly.

### doi

If you store article doi codes in your entries, in the field `doi`, `bibtex-utils` can look those up for you to. At the moment, this only works with firefox with the `doi protocol` extension, but it may be possible to tweak it to use Chrome with a different extension. The variables to play with are `bu-doi-resolver` and `bu-doi-prefix`. Following a doi link will invoke the command `bu-doi-resolver bu-doi-prefix doi`, which by default is `firefox doi:10.XXX.XXXX`.

### url

If you store the article url in the url field, `bibtex-utils` can open the url using Emacs' default browser, via the function `browse-url`.

### How it works

When you invoke `bu-open-doc` from anywhere within a bibtex entry, `bibtex-utils` first looks for the pdf and opens it if it finds it. If it doesn't, it will check for a doi, and open that if it exists. Finally, it will try to browse to the url. I'm planning to make the order of priority customizable in the near future.

## bu-jump-to-doc

This function will jump from a bibtex citation in a source document directly to the document itself (i.e., the pdf or webpage). It's a wrapper to `bu-open-doc` that works when you're not already on the bibtex entry.
