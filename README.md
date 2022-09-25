# What is this?
this is transpiler for scheme(currently no full futured) to *any* language(except some languages).

## Usage

```bash
cat "any scheme file" | gosh simplify.scm | gosh cps.scm | gosh lua.scm # output is lua program
```

## etc

### simplify.scm
This simplify scheme program likely below.
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
This convert program from direct style to continuation-passing style(CPS). It only accept program modified by `simplify.scm`

### lua.scm
This is transpiler from scheme to lua. You can duplicate this file and modify to make transpiler which convert to any language without creating hard operator (ex. `call/cc`)

### about CPS
To treat `call/cc`, all of function call will be modified. So, when you write function call, first argument will be inserted. To use not a scheme functions, please use `transpiler-ffi` likely below.

```lisp
;; This ffi is made for Lua
(transpiler-ffi
 "
function mysin(cont,x)
  return cont(math.sin(x))
end

function myprint(cont,x)
  print(x)
  return cont(1)
end
"
 )
 
;; And you can call that
(myprint (mysin 1.57))
```
