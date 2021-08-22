-- To run .exe, you do: .\first.exe
main = putStrLn "Hello, World!"
size:: Int
size = 12+13

--cuadrado::Int -> Int    --Firma
cuadrado n = n*n        --Funcion

suma::Int -> Int -> Int
suma a b = a + b        --Funcion

eligeSaludo x = if x then "adios"
                else "hola"
