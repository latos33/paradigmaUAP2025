module Main exposing (..)

import Html exposing (Html, text)


main : Html msg
main =
    text "Hello, Elm!"


add : Int -> Int -> Int
add a b =
    a + b


multiply : Int -> Int -> Int
multiply a b =
    a * b


-- Ejercicio 1: Función Potencia
power : Int -> Int -> Int
power a b =
if b == 0 then
        1
    else if b < 0 then
        0 -- No se manejan exponentes negativos en esta implementación
    else
        a * power a (b - 1)
    -- TODO: Implementar función potencia
    


-- Ejercicio 2: Factorial
factorial : Int -> Int
factorial n =
    -- TODO: Implementar factorial
    0


-- Ejercicio 3: Fibonacci
fibonacciExponential : Int -> Int
fibonacciExponential n =             
if n <= 0 then
        0
    else if n == 1 then
        1
    else
        fibonacciExponential (n - 1) + fibonacciExponential (n - 2)

    -- TODO: Implementar fibonacci exponencial

    


fibonacciLinear : Int -> Int
fibonacciLinear n =
    -- TODO: Implementar fibonacci lineal con acumuladores
    fibonacciHelper n 0 1


    


fibonacciHelper : Int -> Int -> Int -> Int
fibonacciHelper n acc1 acc2 =
    -- TODO: Función auxiliar para fibonacci lineal
    if n == 0 then
        acc1
    else
        fibonacciHelper (n - 1) acc2 (acc1 + acc2)


-- Ejercicio 4: Triángulo de Pascal
pascalTriangle : Int -> Int -> Int
pascalTriangle x y =
    -- TODO: Implementar triángulo de Pascal
    if y == 0 || y == x then
        1
    else
        pascalTriangle (x - 1) (y - 1) + pascalTriangle (x - 1) y


-- Ejercicio 5: Máximo Común Divisor (MCD)
gcd : Int -> Int -> Int
gcd a b = 
    -- TODO: Implementar algoritmo euclidiano
    
    if b == 0 then
        a
    else
        gcd b (modBy b a)

-- Ejercicio 6: Contar Dígitos
countDigits : Int -> Int
countDigits n =
    -- TODO: Implementar contador de dígitos
    if n < 10 then 1
    else countDigits(n // 10)


-- Ejercicio 7: Suma de Dígitos
sumDigits : Int -> Int
sumDigits n =
    -- TODO: Implementar suma de dígitos
    if n < 10 then n
    else modBy 10 n + sumDigits(n // 10)


-- Ejercicio 8: Verificar Palíndromo
isPalindrome : Int -> Bool
isPalindrome n =  False
 
    -- TODO: Implementar verificador de palíndromo



reverseNumber : Int -> Int
reverseNumber n =

    -- TODO: Implementar función para invertir número
    reverseHelper n 0




reverseHelper : Int -> Int -> Int
reverseHelper n acc =
    -- TODO: Función auxiliar para invertir número
    if n < 10 then acc * 10 + n
   

    else reverseHelper (n // 10) (acc * 10 + modBy 10 n) 



-- Ejercicio 9: Paréntesis Balanceados
isBalanced : String -> Bool
isBalanced str =
    -- TODO: Implementar verificador de paréntesis balanceados
    False


isBalancedHelper : List Char -> Int -> Bool
isBalancedHelper chars counter =
    -- TODO: Función auxiliar para verificar paréntesis balanceados
    False