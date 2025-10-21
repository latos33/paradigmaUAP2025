module Clase3 exposing (..)


head : List a -> a
head list =
    case List.head list of
        Just h ->
            h

        Nothing ->
            Debug.todo "head called on empty list"


tail : List a -> List a
tail list =
    Maybe.withDefault [] (List.tail list)


isEmpty : List a -> Bool
isEmpty list =
    List.isEmpty list


{-| Ejercicios de Programación Funcional - Clase 3
Este módulo contiene ejercicios para practicar funciones de orden superior en Elm.
Cada función debe implementarse usando principios de programación funcional.

Nota: Las funciones que podrían fallar devuelven valores por defecto (0)
en lugar de usar Maybe. Trabajamos con List de Elm.

-}


-- ============================================================================
-- PARTE 0: IMPLEMENTACIONES PERSONALIZADAS
-- ============================================================================
-- Implementá tus propias versiones de map, filter y fold usando recursión


-- 1. Map Personalizado
-- Implementá tu propia versión de map usando recursión y las funciones genéricas head, tail e isEmpty


miMap : (a -> b) -> List a -> List b
miMap fx lista =
    if isEmpty lista then []
    else
        fx (head lista) :: miMap fx (tail lista) -- :: es el operador de cons en elm que agrega un elemento al inicio de la lista



-- 2. Filter Personalizado
-- Implementá tu propia versión de filter usando recursión


miFiltro : (a -> Bool) -> List a -> List a
miFiltro predicado lista =
    if isEmpty lista then []
        else if predicado (head lista) then
        let h = (head lista) in
        if predicado h then h :: (miFiltro predicado(tail lista))
        else miFiltro predicado (tail lista)
    else miFiltro predicado (tail lista) 


-- 3. Foldl Personalizado
-- Implementá tu propia versión de foldl usando recursión


miFoldl : (a -> b -> b) -> b -> List a -> b
miFoldl fx acumulador lista =
    if isEmpty lista then acumulador
    else let nuevoAcumulador = fx (head lista) acumulador in
        miFoldl fx nuevoAcumulador (tail lista)
        


-- ============================================================================
-- PARTE 1: ENTENDIENDO MAP
-- ============================================================================


-- 4. Duplicar Números
-- Escribí una función que duplique cada número en una lista


duplicar : List Int -> List Int
duplicar lista =
    miMap(\elem -> elem * 2) lista


-- 5. Longitudes de Strings
-- Convertí una lista de strings a una lista de sus longitudes


longitudes : List String -> List Int
longitudes lista =
    miMap(\elem -> elem.length) lista

 
    



-- 6. Incrementar Todos
-- Sumá 1 a cada número en una lista


incrementarTodos : List Int -> List Int
incrementarTodos lista =
   miMap(\elem-> elem + 1) [] lista 



-- 7. A Mayúsculas
-- Convertí todos los strings de una lista a mayúsculas


todasMayusculas : List String -> List String
todasMayusculas lista =
    miMap(\elem -> String.toUpper elem) [] lista



-- 8. Negar Booleanos
-- Invertí todos los valores booleanos en una lista


negarTodos : List Bool -> List Bool
negarTodos lista =
    miMap(\elem -> not elem) [] lista -- El operador not infierte un valor booleano



-- ============================================================================
-- PARTE 2: ENTENDIENDO FILTER
-- ============================================================================


-- 9. Números Pares
-- Mantené solo los números pares de una lista


pares : List Int -> List Int
pares lista =
    miFiltro (\elem -> elem % 2 == 0) lista


-- 10. Números Positivos
-- Mantené solo los números positivos


positivos : List Int -> List Int
positivos lista =
    miFiltro (\elem -> elem > 0) lista



-- 11. Strings Largos
-- Mantené solo los strings con más de 5 caracteres


stringsLargos : List String -> List String
stringsLargos lista =
     miFiltro (\elem -> String.length elem > 5) lista
    



-- 12. Remover Falsos
-- Remové todos los valores False de una lista de booleanos


soloVerdaderos : List Bool -> List Bool
soloVerdaderos lista =
    miFiltro (\elem -> elem == True) lista



-- 13. Mayor Que
-- Filtrá números mayores que un valor dado


mayoresQue : Int -> List Int -> List Int
mayoresQue valor lista =
    miFiltro (\elem -> elem > valor) lista



-- ============================================================================
-- PARTE 3: ENTENDIENDO FOLD
-- ============================================================================


-- 14. Suma con Fold
-- Implementá suma usando List.foldl


sumaFold : List Int -> Int
sumaFold lista =
    miFoldl (\elem acum -> elem + acum) 0 lista
    



-- 15. Producto
-- Multiplicá todos los números de una lista entre sí


producto : List Int -> Int
producto lista =
    miFoldl (\elem acum -> elem*acum) 0 lista



-- 16. Contar con Fold
-- Implementá contar usando List.foldl


contarFold : List a -> Int
contarFold lista =
    miFoldl (\elem acum -> acum+1) 0 lista



-- 17. Concatenar Strings
-- Uní todos los strings de una lista


concatenar : List String -> String
concatenar lista =
    miFoldl (\elem acum -> acum ++ elem) "" lista



-- 18. Valor Máximo
-- Encontrá el valor máximo en una lista de números (devolvé 0 para lista vacía)


maximo : List Int -> Int
maximo lista =
    

miFoldl (\elem acum -> if elem > acum then elem else acum) 0 listas
-- 19. Invertir con Fold
-- Invertí una lista usando List.foldl


invertirFold : List a -> List a
invertirFold lista =
    miFoldl (\elem acum -> elem :: acum) [] lista



-- 20. Todos Verdaderos
-- Verificá si todos los elementos de una lista satisfacen una condición


todos : (a -> Bool) -> List a -> Bool
todos predicado lista =
    miFoldl(\elem acum -> if predicado elem then acum else False) True lista

    


-- 21. Alguno Verdadero
-- Verificá si al menos un elemento satisface una condición


alguno : (a -> Bool) -> List a -> Bool
alguno predicado lista =
    miFoldl(\elem acum -> if predicado elem then True else acum) False lista



-- ============================================================================
-- PARTE 4: COMBINANDO OPERACIONES
-- ============================================================================


-- 22. Suma de Cuadrados
-- Calculá la suma de los cuadrados de todos los números


sumaDeCuadrados : List Int -> Int
sumaDeCuadrados lista =
    let cuadrados = miMap(\elem -> elem * elem) 
    in
    miFoldl(\elem acum -> acum + elem) 0 cuadrados


-- 23. Contar Números Pares
-- Contá cuántos números pares hay en una lista


contarPares : List Int -> Int
contarPares lista =
    let pares = miFiltro(\elem -> elem % 2 == 0) lista
    in
    miFoldl(\elem acum -> acum +1) 0 pares


-- 24. Promedio
-- Calculá el promedio de una lista de números (devolvé 0 para lista vacía)


promedio : List Float -> Float
promedio lista =
    if isEmpty lista then 0
    else
        let suma = miFoldl(\elem acum -> acum + elem) 0 lista
            cantidad = miFoldl(\elem acum -> acum + 1) 0 lista
        in
        suma / toFloat cantidad



-- 25. Palabras a Longitudes
-- Dada una oración (string), dividila en palabras y devolvé sus longitudes


longitudesPalabras : String -> List Int
longitudesPalabras oracion =
    let palabras = String.words oracion
    in
    miMap(\elem -> String.length elem) palabras



-- 26. Remover Palabras Cortas
-- Mantené solo las palabras con más de 3 caracteres de una oración


palabrasLargas : String -> List String
palabrasLargas oracion =
    let palabras = String.words oracion
    in
    miFIltro(/elem -> String.length elem > 3) palabras



-- 27. Sumar Números Positivos
-- Sumá solo los números positivos de una lista


sumarPositivos : List Int -> Int
sumarPositivos lista =
    let positivos = miFiltro(\elem -> elem > 0) lista
    in
    miFoldl(\elem acum -> acum + elem) 0 positivos


-- 28. Duplicar Pares
-- Duplicá solo los números pares de una lista, mantené los impares sin cambios


duplicarPares : List Int -> List Int
duplicarPares lista =
    let pares = miFiltro(\elem -> elem % 2 == 0) lista
    in
    miMap(\elem -> elem * 2) pares
    



-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 29. Aplanar
-- Aplaná una lista de listas en una única lista


aplanar : List (List a) -> List a
aplanar lista =

  case lista of
    [] -> [] 
    h::t -> h ++ (aplanarLista t)




-- 30. Agrupar Por
-- Agrupá elementos por una función clave (devolvé lista de listas)
-- ¡Esto es desafiante! Agrupá elementos iguales consecutivos juntos


agruparPor : (a -> a -> Bool) -> List a -> List (List a)
agruparPor comparador lista = 
    case lista of
        [] ->
            []

        x :: xs ->
            let
                (iguales, resto) =
                    span (\y -> y == x) xs
            in
            (x :: iguales) :: agruparPor resto

    



-- 31. Particionar
-- Separá una lista en dos listas basándote en un predicado


particionar : (a -> Bool) -> List a -> ( List a, List a )
particionar predicado lista =
    ( miFiltro predicado lista, miFiltro (\elem -> not (predicado elem)) lista )



-- 32. Suma Acumulada
-- Creá una lista de sumas acumuladas


sumaAcumulada : List Int -> List Int
sumaAcumulada lista =
    case lista of
        [] -> []
        x :: xs ->
            let
                parcial = sumaAcumulada xs
            in
            case parcial of
                [] ->
                    [x]
                y :: _ ->
                    x :: miMap (\n -> n + x) parcial



-- ============================================================================
-- EJERCICIOS OPCIONALES
-- ============================================================================


-- Subconjuntos
-- Generá todos los posibles subconjuntos de una lista


subSets : List Int -> List (List Int)
subSets lista =
    case lista of
        [] ->
            [ [] ]

        x :: xs ->
            []



-- Dividir en Grupos
-- Dividí una lista en grupos de tamaño n


cortar : List Int -> Int -> List (List Int)
cortar lista n =
    if isEmpty lista then
        []

    else
        tomar n lista :: cortar (saltar n lista) n



-- Función auxiliar para tomar los primeros n elementos de una lista


tomar : Int -> List a -> List a
tomar n lista =
    if isEmpty lista then
        []

    else if n == 0 then
        []

    else
        head lista :: tomar (n - 1) (tail lista)



-- Función auxiliar para saltar los primeros n elementos de una lista


saltar : Int -> List a -> List a
saltar n lista =
    if isEmpty lista then
        []

    else if n == 0 then
        lista

    else
        saltar (n - 1) (tail lista)
