(transpiler-ffi
 "
local myprint = function(cont,...)
    print(...)
    return cont(1)
end

local mysin = function(cont,x)
    return cont(math.sin(x))
end
"
 )

(myprint "Hello World!")
(myprint "The sin value at 0.5 is" (mysin 0.5))
