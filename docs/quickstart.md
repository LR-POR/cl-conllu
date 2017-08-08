# `cl-conllu` quickstart

<!-- a biblioteca funciona em outras implementações além de sbcl? -->

`cl-conllu` is a Common Lisp library to work with
[CoNLL-U](http://universaldependencies.org/format.html "CoNLL-U format specification")
files, licensed under the 
[Apache license](http://www.apache.org/licenses/LICENSE-2.0).

## quicklisp

If don't have quicklisp installed already,
follow [these steps](https://www.quicklisp.org/beta/#installation).

The `cl-conllu` library is not available from quicklisp's servers, so
you must clone this project to your `local-projects` quicklisp
directory (usually at `~/quicklisp/local-projects/`). Navigate there
and

- if you'll be developing the library, you should clone your fork of
  the library. So fork the library, and do
  
		git clone https://github.com/your-username/cl-conllu.git

- if you just want to use it, you can download
  the [.zip](https://github.com/own-pt/cl-conllu/archive/master.zip)
  of the repository, and unpack it.

After downloading the project to your `local-projects` directory, you
just have to load the library. Do

	(ql:quickload "cl-conllu")

and quicklisp will download the library's dependencies and then load it.

## reading CoNLL-U files

First off, you need some CoNLL-U files to start with. If you have none
in mind, you may get some from
[here](https://github.com/own-pt/bosque-UD/tree/master/documents).

The simplest way to read a file with `cl-conllu` is with `read-file`:

``` common-lisp
CL-USER> (defparameter *sents* nil)
CL-USER> (setf *sents* (cl-conllu:read-file #p"/path/to/my/file/CF1.conllu"))
(#<CL-CONLLU:SENTENCE {1003CFD9C3}> #<CL-CONLLU:SENTENCE {1003D164C3}>
 #<CL-CONLLU:SENTENCE {1003D1BB13}> #<CL-CONLLU:SENTENCE {1003D2C013}>
 #<CL-CONLLU:SENTENCE {1003D348A3}> #<CL-CONLLU:SENTENCE {1003D3E383}>
 #<CL-CONLLU:SENTENCE {1003D49C23}>)
```

Each object returned is a `sentence` object, made up of `token`
objects, which we will describe in the next section.

Other convenient functions are `read-directory` and
`read-stream`. There's also an umbrella function, `read-conllu`, which
will try to guess the input you passed and call the appropriate
function to read it.

All read functions accept a `fn-meta` function as argument. This
function collects the metadata from each CoNLL-U sentence, which
usually includes the raw sentence (see
the [reference](http://universaldependencies.org/format.html)). The
default metadata collector function, `collect-meta`, is defined for
the metadata scheme used by the 
[OpenWordnet-PT](http://wnpt.brlcloud.com/wn/).

## `cl-conllu` classes

`cl-conllu` has a few central classes: `sentence`s, `token`s, and
`mtoken`s. They are all defined in
[data.lisp](https://github.com/own-pt/cl-conllu/blob/master/data.lisp). 
When a CoNLL-U file is read, its contents are turned into instances of
these classes.

### sentences

Every CoNLL-U sentence is turned in an instance of the `sentence`
class by `cl-conllu`. Each instance is characterized by four
properties: `start`, `meta`, `tokens`, and `mtokens`.  <!-- why is
start included if it's always 1? (right? wrong. what is it??) -->

`start` is...
 
`meta` includes the metainformation regarding the sentence. this may
vary, as we have discussed in the previous section, but usually
includes the full (raw) sentence and the sentence ID, as required by
the CoNLL-U format specification.

``` common-lisp
CL-USER> (cl-conllu:sentence-meta (first *sents*))
(("text" . "PT no governo")
 ("source" . "CETENFolha n=1 cad=Opinião sec=opi sem=94a")
 ("sent_id" . "CF1-1") ("id" . "1"))
```

The `tokens` are the list of tokens that together form the sentence,
and they are themselves instances of the `token` class.

The `mtokens` (meta-tokens) are also instances of their own `mtoken`
class, and they encapsulate both the multiword tokens ("vámonos =
vamos + nos") and the empty tokens ("Sue likes coffee and Bill
(*likes*) tea") defined by the CoNLL-U format specification
[here](http://universaldependencies.org/format.html#words-tokens-and-empty-nodes).

### tokens

Instances of the `token` class have one property for each field/column
in the CoNLL-U format's sentences, that is:

> 1. ID: Word index, integer starting at 1 for each new sentence; may be
>    a range for multiword tokens; may be a decimal number for empty
>    nodes.
> 2. FORM: Word form or punctuation symbol.
> 3. LEMMA: Lemma or stem of word form.
> 4. UPOSTAG: Universal part-of-speech tag.
> 5. XPOSTAG: Language-specific part-of-speech tag; underscore if not
>    available.
> 6. FEATS: List of morphological features from the universal feature
>    inventory or from a defined language-specific extension; underscore
>    if not available.
> 7. HEAD: Head of the current word, which is either a value of ID or
>    zero (0).
> 8. DEPREL: Universal dependency relation to the HEAD (root iff HEAD = 0)
>    or a defined language-specific subtype of one.
> 9. DEPS: Enhanced dependency graph in the form of a list of
>    head-deprel pairs.
> 10. MISC: Any other annotation.

for instance,

``` common-lisp
CL-USER> (mapcar #'cl-conllu:token-form (cl-conllu:sentence-tokens (first *frases*)))
("PT" "em" "o" "governo")
CL-USER> (mapcar #'cl-conllu:token-feats (cl-conllu:sentence-tokens (first *frases*)))
("Gender=Masc|Number=Sing" "_"
 "Definite=Def|Gender=Masc|Number=Sing|PronType=Art" "Gender=Masc|Number=Sing")
```

## visualizing CoNLL-U sentences

To visualize CoNLL-U sentences, we use the `conllu-visualize`
subpackage. The function `tree-sentence` receives an instance of a
sentence object and (optionally) an output stream, and outputs to the
stream the sentence's metadata and its tree structure:

``` common-lisp
CL-USER> (conllu-visualize:tree-sentence (nth 5 *frases*))
text = Eles se dizem oposição, mas ainda não informaram o que vão combater.
source = CETENFolha n=1 cad=Opinião sec=opi sem=94a
sent_id = CF1-7
id = 6
─┮ 
 │ ╭─╼ Eles nsubj 
 │ ├─╼ se expl 
 ╰─┾ dizem root 
   ├─╼ oposição xcomp 
   │ ╭─╼ , punct 
   │ ├─╼ mas cc 
   │ │ ╭─╼ ainda advmod 
   │ ├─┶ não advmod 
   ├─┾ informaram conj 
   │ │   ╭─╼ o det 
   │ │ ╭─┶ que obj 
   │ │ ├─╼ vão aux 
   │ ╰─┶ combater ccomp 
   ╰─╼ . punct 
```

## querying CoNLL-U files

