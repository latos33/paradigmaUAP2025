module Clase4 exposing (..)

{-| Ejercicios de Programación Funcional - Clase 4
Este módulo contiene ejercicios para practicar pattern matching y mónadas en Elm
usando árboles binarios como estructura de datos principal.

Temas:
- Pattern Matching con tipos algebraicos
- Mónada Maybe para operaciones opcionales
- Mónada Result para manejo de errores
- Composición monádica con andThen
-}
import Platform.Sub exposing (none)


-- ============================================================================
-- DEFINICIÓN DEL ÁRBOL BINARIO
-- ============================================================================

type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


-- ============================================================================
-- PARTE 0: CONSTRUCCIÓN DE ÁRBOLES
-- ============================================================================


-- 1. Crear Árboles de Ejemplo


arbolVacio : Tree Int
arbolVacio =
    Empty


arbolHoja : Tree Int
arbolHoja =
    Node 5 Empty Empty


arbolPequeno : Tree Int
arbolPequeno =
    Node 3 (Node 1 Empty Empty) (Node 5 Empty Empty)


arbolMediano : Tree Int
arbolMediano =
    Node 10
        (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
        (Node 15 (Node 12 Empty Empty) (Node 20 Empty Empty))


-- 2. Es Vacío


esVacio : Tree a -> Bool
esVacio arbol =
    case arbol of 

        Empty -> True
        Node _ _ _ -> False


-- 3. Es Hoja


esHoja : Tree a -> Bool
esHoja arbol =
    case arbol of 
        Node _ Empty Empty -> True
        _ -> False


-- ============================================================================
-- PARTE 1: PATTERN MATCHING CON ÁRBOLES
-- ============================================================================


-- 4. Tamaño del Árbol


tamano : Tree a -> Int
tamano arbol =
    case arbol of
        Empty -> 0
        Node _ left right -> (tamano left) + (tamano right) + 1


-- 5. Altura del Árbol


altura : Tree a -> Int
altura arbol =
    case arbol of
        Empty -> 0
        Node _ izq der -> (max (altura izq)(altura der)) + 1 


-- 6. Suma de Valores


sumarArbol : Tree Int -> Int
sumarArbol arbol =
    case arbol of 
        Empty -> 0
        Node valor izq der -> (sumarArbol izq) + (sumarArbol der) + valor


-- 7. Contiene Valor


contiene : a -> Tree a -> Bool
contiene valor arbol =
    case arbol of
        Empty -> False
        Node v izq der -> if v == valor || (contiene valor izq) || (contiene valor der)
                        then True
                        else False


-- 8. Contar Hojas


contarHojas : Tree a -> Int
contarHojas arbol =
    case arbol of 
        Empty -> 0
        Node _ Empty Empty -> 1
        Node _ izq der -> (contarHojas izq) + (contarHojas der)


-- 9. Valor Mínimo (sin Maybe)


minimo : Tree Int -> Int
minimo arbol =
    case arbol of
        Empty -> 0
        Node v Empty Empty -> v
        Node v Empty der -> (min v (min der))
        Node v izq Empty -> (min v (min izq))
        Node v izq der -> (min v (min (minimo izq) (minimo der)))


-- 10. Valor Máximo (sin Maybe)


maximo : Tree Int -> Int
maximo arbol =
    case arbol of
        Empty -> 0
        Node v Empty Empty -> v
        Node v Empty der -> (min v (min der))
        Node v izq Empty -> (min v (min izq))
        Node v izq der -> (max v (max (maximo izq) (maximo der)))


-- ============================================================================
-- PARTE 2: INTRODUCCIÓN A MAYBE
-- ============================================================================


-- 11. Buscar Valor

buscar : a -> Tree a -> Maybe a
buscar valor arbol =
    case arbol of
        Empty -> Nothing
        Node v izq der -> 
            if v == valor 
                then Just v     
                else 
                    case buscar valor izq of 
                        Just v -> Just v 
                        Nothing -> buscar valor der 


-- 12. Encontrar Mínimo (con Maybe)


encontrarMinimo : Tree Int -> Maybe Int
encontrarMinimo arbol =
    case arbol of 
        Empty -> Nothing
        Node v Empty Empty -> Just v 
        Node v izq der -> case ((encontrarMinimo izq), (encontrarMinimo der)) of
            (Nothing, Nothing) -> Just v
            (Just minIzq, Nothing) -> Just (min v minIzq)
            (Nothing, Just minDer) -> Just (min v minDer)
            (Just minIzq, Just minDer) -> Just (min v(min minIzq minDer))
        


-- 13. Encontrar Máximo (con Maybe)


encontrarMaximo : Tree comparable -> Maybe comparable
encontrarMaximo arbol =
    case arbol of 
        Empty -> Nothing
        Node v Empty Empty -> Just v 
        Node v izq der -> case ((encontrarMaximo izq), (encontrarMaximo der)) of
            (Nothing, Nothing) -> Just v
            (Just maxIzq, Nothing) -> Just (max v maxIzq)
            (Nothing, Just maxDer) -> Just (max v maxDer)
            (Just maxIzq, Just maxDer) -> Just (max v(max maxIzq maxDer))
        


-- 14. Buscar Por Predicado


buscarPor : (a -> Bool) -> Tree a -> Maybe a
buscarPor predicado arbol =
   
    case arbol of
        Empty -> Nothing
        Node v izq der -> 
            if predicado v 
                then Just v     
                else 
                    case buscarPor predicado izq of 
                        Just v -> Just v 
                        Nothing -> buscarPor predicado der
                        
    


-- 15. Obtener Valor de Raíz


raiz : Tree a -> Maybe a
raiz arbol =
    case arbol of 
        Empty -> Nothing
        Node v _ _ -> Just v 


-- 16. Obtener Hijo Izquierdo


hijoIzquierdo : Tree a -> Maybe (Tree a)
hijoIzquierdo arbol =
    case arbol of
        Empty -> Nothing
        Node _ Empty _ -> Nothing
        Node _ izq _ -> Just izq 


hijoDerecho : Tree a -> Maybe (Tree a)
hijoDerecho arbol =
    case arbol of
        Empty -> Nothing
        Node _ _ Empty -> Nothing
        Node _ _ der -> Just der 


-- 17. Obtener Nieto


nietoIzquierdoIzquierdo : Tree a -> Maybe (Tree a)
nietoIzquierdoIzquierdo arbol =
    case arbol of
        Node _ (Node _ nieto _) _ ->
            Just nieto

        _ ->Nothing


-- 18. Buscar en Profundidad


obtenerSubarbol : a -> Tree a -> Maybe (Tree a)
obtenerSubarbol valor arbol =
    case arbol of 
        Empty -> Nothing
        Node v izq der -> 
            if v == valor 
                then Just arbol
                else 
                    case obtenerSubarbol valor izq of 
                        Just subarbol -> Just subarbol
                        Nothing -> obtenerSubarbol valor der


buscarEnSubarbol : a -> a -> Tree a -> Maybe a
buscarEnSubarbol valor1 valor2 arbol =
    case obtenerSubarbol valor1 arbol of 
        Just subarbol -> buscar valor2 subarbol
        Nothing -> Nothing
        


-- ============================================================================
-- PARTE 3: RESULT PARA VALIDACIONES
-- ============================================================================


-- 19. Validar No Vacío


validarNoVacio : Tree a -> Result String (Tree a)
validarNoVacio arbol =
    case arbol of 
    Empty -> Err "El árbol está vacío"
    Node _ _ _ -> Ok arbol



-- 20. Obtener Raíz con Error


obtenerRaiz : Tree a -> Result String a
obtenerRaiz arbol =
    case arbol of 
    Empty -> Err "No se puede obtener la raíz de un árbol vacío"
    Node v _ _ -> Ok v


-- 21. Dividir en Valor Raíz y Subárboles


dividir : Tree a -> Result String ( a, Tree a, Tree a )
dividir arbol =
    case arbol of 
    Empty -> Err "No se puede dividir un árbol vacío"
    Node v izq der -> Ok (v, izq, der)



-- 22. Obtener Mínimo con Error


obtenerMinimo : Tree comparable -> Result String comparable
obtenerMinimo arbol =
    case arbol of
    Empty -> Err "No hay mínimo en un árbol vacío"
    Node v Empty Empty -> Ok v
    Node v izq der -> 
        case ((obtenerMinimo izq), (obtenerMinimo der)) of
            (Err e, Err f) -> Ok v
            (Ok minIzq, Err f) -> Ok (min v minIzq)
            (Err e, Ok minDer) -> Ok (min v minDer)
            (Ok minIzq, Ok minDer) -> Ok (min v(min minIzq minDer))


-- 23. Verificar si es BST


esBST : Tree comparable -> Bool
esBST arbol =
    esBSTConRango arbol Nothing Nothing


esBSTConRango : Tree comparable -> Maybe comparable -> Maybe comparable -> Bool
esBSTConRango arbol minVal maxVal =
    case arbol of
        Empty ->
            True

        Node valor izq der ->
            let
                dentroDeRango =
                    case minVal of
                        Just minimo ->
                            valor > minimo

                        Nothing ->
                            True
            in
            if not dentroDeRango then
                False
            else
                let
                    dentroDeRango2 =
                        case maxVal of
                            Just maximo ->
                                valor < maximo

                            Nothing ->
                                True
                in
                if not dentroDeRango2 then
                    False
                else
                    esBSTConRango izq minVal (Just valor)
                        && esBSTConRango der (Just valor) maxVal



-- 24. Insertar en BST


insertarBST : comparable -> Tree comparable -> Result String (Tree comparable)
insertarBST valor arbol =
    case arbol of
        Empty ->
            Ok (Node valor Empty Empty)

        Node v izq der ->
            if valor == v then
                Err "El valor ya existe en el árbol"

            else if valor < v then
                case insertarBST valor izq of
                    Ok nuevoIzq ->
                        Ok (Node v nuevoIzq der)

                    Err mensaje ->
                        Err mensaje

            else
                case insertarBST valor der of
                    Ok nuevoDer ->
                        Ok (Node v izq nuevoDer)

                    Err mensaje ->
                        Err mensaje
    


-- 25. Buscar en BST


buscarEnBST : comparable -> Tree comparable -> Result String comparable
buscarEnBST valor arbol =
    case arbol of
        Empty ->
            Err "El valor no se encuentra en el árbol"

        Node v izq der ->
            if valor == v then
                Ok v

            else if valor < v then
                buscarEnBST valor izq

            else
                buscarEnBST valor der


-- 26. Validar BST con Result


validarBST : Tree comparable -> Result String (Tree comparable)
validarBST arbol =
    let
        esValido tree minBound maxBound =
            case tree of
                Empty ->
                    True

                Node val left right ->
                    (case minBound of
                        Just min -> val > min
                        Nothing -> True
                    )
                    && (case maxBound of
                            Just max -> val < max
                            Nothing -> True
                        )
                    && esValido left minBound (Just val)
                    && esValido right (Just val) maxBound
    in
    if esValido arbol Nothing Nothing then
        Ok arbol
    else
        Err "El árbol no es un BST válido"


-- ============================================================================
-- PARTE 4: COMBINANDO MAYBE Y RESULT
-- ============================================================================


-- 27. Maybe a Result


maybeAResult : String -> Maybe a -> Result String a
maybeAResult mensajeError maybe =
    case maybe of
        Just valor ->
            Ok valor

        Nothing ->
    Err mensajeError



-- 28. Result a Maybe


resultAMaybe : Result error value -> Maybe value
resultAMaybe result =
    case result of
        Ok v -> Just v 
        err _ -> Nothing


-- 29. Buscar y Validar


buscarPositivo : Int -> Tree Int -> Result String Int
buscarPositivo valor arbol =
    case buscarEnBST valor arbol of
        Ok v ->
            if v > 0 then
                Ok v
            else
                Err "El valor no es positivo"

        Err _ ->
    Err "El valor no se encuentra en el árbol"


-- 30. Pipeline de Validaciones


validarArbol : Tree Int -> Result String (Tree Int)
validarArbol arbol =
    case arbol of 
        Node v izq der ->
            if v > 0 then
                Ok arbol
            else
                Err "Validación fallida"
        Empty ->
    Err "Validación fallida"


-- 31. Encadenar Búsquedas


buscarEnDosArboles : Int -> Tree Int -> Tree Int -> Result String Int
buscarEnDosArboles valor arbol1 arbol2 =
    case buscarEnBST valor arbol1 of
        Ok v ->
            Ok v

        Err _ ->
            case buscarEnBST valor arbol2 of
                Ok v2 ->
                    Ok v2

                Err _ ->
    Err "Búsqueda fallida"


-- ============================================================================
-- PARTE 5: DESAFÍOS AVANZADOS
-- ============================================================================


-- 32. Recorrido Inorder


inorder : Tree a -> List a
inorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> (inorder izq) ++ [v] ++ (inorder der)


-- 33. Recorrido Preorder


preorder : Tree a -> List a
preorder arbol =
    case arbol of
        Empty -> []
        Node v izq der -> [v] ++ (preorder izq) ++ (preorder der)


-- 34. Recorrido Postorder


postorder : Tree a -> List a
postorder arbol =
    case arbol of
        Empty -> []
        Node v izq der ->  (postorder izq) ++ (postorder der) ++ [v]


-- 35. Map sobre Árbol


mapArbol : (a -> b) -> Tree a -> Tree b
mapArbol funcion arbol =
    case arbol of
        Empty -> Empty
        Node v izq der -> Node (funcion v) (mapArbol funcion izq) (mapArbol funcion der)


-- 36. Filter sobre Árbol


filterArbol : (a -> Bool) -> Tree a -> Tree a
filterArbol predicado arbol =
    case arbol of
        Empty -> Empty
        Node v izq der ->
            let
                nuevoIzq = filterArbol predicado izq
                nuevoDer = filterArbol predicado der
            in
            if predicado v then
                Node v nuevoIzq nuevoDer
            else
                arbolfilt nuevoIzq nuevoDer



-- 37. Fold sobre Árbol


foldArbol : (a -> b -> b) -> b -> Tree a -> b
foldArbol funcion acumulador arbol =
    case arbol of
        Empty -> acumulador
        Node v izq der ->
            let
                acumuladorIzq = foldArbol funcion acumulador izq
                acumuladorDer = foldArbol funcion acumuladorIzq der
            in
            funcion v acumulador


-- 38. Eliminar de BST


eliminarBST : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarBST valor arbol =
    case arbol of
        Empty ->
            Err "El valor no existe en el árbol"

        Node v izq der ->
            if valor < v then
                case eliminarBST valor izq of
                    Ok nuevoIzq ->
                        Ok (Node v nuevoIzq der)

                    Err mensaje ->
                        Err mensaje

            else if valor > v then
                case eliminarBST valor der of
                    Ok nuevoDer ->
                        Ok (Node v izq nuevoDer)

                    Err mensaje ->
                        Err mensaje

            else
                -- Valor encontrado, proceder a eliminar
                case (izq, der) of
                    (Empty, Empty) ->
                        Ok Empty

                    (Empty, _) ->
                        Ok der

                    (_, Empty) ->
                        Ok izq

                    _ ->
                        case encontrarMinimo der of
                            Just minDer ->
                                case eliminarBST minDer der of
                                    Ok nuevoDer ->
                                        Ok (Node minDer izq nuevoDer)

                                    Err mensaje ->
                                        Err mensaje

                            Nothing ->
    Err "El valor no existe en el árbol"



-- 39. Construir BST desde Lista


desdeListaBST : List comparable -> Result String (Tree comparable)
desdeListaBST lista =
    case lista of 
        [] -> Ok Empty
        x :: xs ->
            case desdeListaBST xs of
                Ok arbol ->
                    case insertarBST x arbol of
                        Ok nuevoArbol ->
                            Ok nuevoArbol

                        Err mensaje ->
                            Err mensaje

                Err mensaje ->
    Err "Valor duplicado"


-- 40. Verificar Balance


estaBalanceado : Tree a -> Bool
estaBalanceado arbol =
    case arbol of
        Empty -> True
        Node _ izq der ->
            let
                alturaIzq = altura izq
                alturaDer = altura der
                diferencia = abs (alturaIzq - alturaDer)
            in
            diferencia <= 1 && estaBalanceado izq && estaBalanceado der



-- 41. Balancear BST


balancear : Tree comparable -> Tree comparable
balancear arbol =
    case arbol of
        Empty -> Empty
        _ ->
            let
                listaOrdenada = inorder arbol

                construirArbol lista =
                    case lista of
                        [] -> Empty
                        _ ->
                            let
                                mid = List.length lista // 2
                                valorMedio = List.drop mid lista |> List.head |> Maybe.withDefault 0
                                izquierda = List.take mid lista
                                derecha = List.drop (mid + 1) lista
                            in
                            Node valorMedio (construirArbol izquierda) (construirArbol derecha)
            in
            construirArbol listaOrdenada


-- 42. Camino a un Valor


type Direccion
    = Izquierda
    | Derecha


encontrarCamino : a -> Tree a -> Result String (List Direccion)
encontrarCamino valor arbol =
    case arbol of
        Empty ->
            Err "El valor no existe en el árbol"

        Node v izq der ->
            if v == valor then
                Ok []
            else
                case encontrarCamino valor izq of
                    Ok caminoIzq ->
                        Ok (Izquierda :: caminoIzq)

                    Err _ ->
                        case encontrarCamino valor der of
                            Ok caminoDer ->
                                Ok (Derecha :: caminoDer)

                            Err _ ->
    Err "El valor no existe en el árbol"


-- 43. Seguir Camino


seguirCamino : List Direccion -> Tree a -> Result String a
seguirCamino camino arbol =
    Err "Camino inválido"


-- 44. Ancestro Común Más Cercano


ancestroComun : comparable -> comparable -> Tree comparable -> Result String comparable
ancestroComun valor1 valor2 arbol =
    Err "Uno o ambos valores no existen en el árbol"


-- ============================================================================
-- PARTE 6: DESAFÍO FINAL - SISTEMA COMPLETO
-- ============================================================================


-- 45. Sistema Completo de BST
-- (Las funciones individuales ya están definidas arriba)


-- Operaciones que retornan Bool
esBSTValido : Tree comparable -> Bool
esBSTValido arbol =
    esBST arbol


estaBalanceadoCompleto : Tree comparable -> Bool
estaBalanceadoCompleto arbol =
    estaBalanceado arbol


contieneValor : comparable -> Tree comparable -> Bool
contieneValor valor arbol =
    contiene valor arbol


-- Operaciones que retornan Maybe
buscarMaybe : comparable -> Tree comparable -> Maybe comparable
buscarMaybe valor arbol =
    buscar valor arbol


encontrarMinimoMaybe : Tree comparable -> Maybe comparable
encontrarMinimoMaybe arbol =
    encontrarMinimo arbol


encontrarMaximoMaybe : Tree comparable -> Maybe comparable
encontrarMaximoMaybe arbol =
    encontrarMaximo arbol


-- Operaciones que retornan Result
insertarResult : comparable -> Tree comparable -> Result String (Tree comparable)
insertarResult valor arbol =
    insertarBST valor arbol


eliminarResult : comparable -> Tree comparable -> Result String (Tree comparable)
eliminarResult valor arbol =
    eliminarBST valor arbol


validarResult : Tree comparable -> Result String (Tree comparable)
validarResult arbol =
    validarBST arbol


obtenerEnPosicion : Int -> Tree comparable -> Result String comparable
obtenerEnPosicion posicion arbol =
    Err "Posición inválida"


-- Operaciones de transformación
map : (a -> b) -> Tree a -> Tree b
map funcion arbol =
    mapArbol funcion arbol


filter : (a -> Bool) -> Tree a -> Tree a
filter predicado arbol =
    filterArbol predicado arbol


fold : (a -> b -> b) -> b -> Tree a -> b
fold funcion acumulador arbol =
    foldArbol funcion acumulador arbol


-- Conversiones
aLista : Tree a -> List a
aLista arbol =
    inorder arbol


desdeListaBalanceada : List comparable -> Tree comparable
desdeListaBalanceada lista =
    Empty
