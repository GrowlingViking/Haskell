--4.4
(or) :: Bool -> Bool -> Bool

--First method
True  or True  = True
True  or False = True
False or True  = True
False or False = False

--Second method
False or False = False
_     or _     = True

--Third method
True  or _ = True
False or b = b

--Fourth method
b or b = b
_ or _ = True
