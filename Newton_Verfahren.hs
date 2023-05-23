--Numerisches Differenzieren (als Funktion)
ableitung f h = \x-> (f(x+h) - f(x)) / h

--Newton Update:
--x_{k+1} = x_k -Df(x_k)^{1} f(x_k)
update x f = x - f x / ableitung f 1e-5 x

--Newton Verfahren:
newton f x = if abs (f x) < 1e-6
    then x else newton f (update x f)
    
