# What is this?
this is transpiler for scheme(currently no full futured) to *any* language(except some languages).

## Usage

```bash
cat "any scheme file" | gosh simplify.scm | gosh cps.scm | gosh lua.scm # output is lua program
```

## etc

### simplify.scm
This simplify scheme program like below.
```lisp
;; from
(let ((x 10))
  (print x))
;; to
((lambda (x)
   (print x))
 10)
 ```

### cps.scm
This convert program from direct style to continuation-passing style(CPS). Currently it only accept program modified by `simplify.scm`

### lua.scm
This is transpiler from scheme to lua. You can duplicate this file and modify to make transpiler which convert to any language without creating hard operator (ex. call/cc)
