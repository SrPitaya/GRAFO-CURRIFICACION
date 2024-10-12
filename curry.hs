import Control.Monad (foldM)
import Data.List (nub)

type Grafo = [[Int]]

main :: IO ()
main = do
    putStrLn "GRAFOS CON USO DE CURRIFICACION"
    putStrLn "¿Cuántos nodos quieres? (mínimo 1)"
    numNodos <- obtenerEntradaValida
    grafo <- crearGrafo numNodos
    imprimirGrafo grafo

obtenerEntradaValida :: IO Int
obtenerEntradaValida = do
    entrada <- getLine
    let num = reads entrada :: [(Int, String)]
    case num of
        [(n, "")] | n >= 1 -> return n
        _ -> do
            putStrLn "Por favor, introduce un número mayor o igual a 1."
            obtenerEntradaValida

crearGrafo :: Int -> IO Grafo
crearGrafo n = do
    putStrLn $ "Introduce conexiones para el grafo de " ++ show n ++ " nodos:"
    let grafoInicial = replicate n []
    foldM (conectarNodos n) grafoInicial [1..n]

conectarNodos :: Int -> Grafo -> Int -> IO Grafo
conectarNodos n grafo nodo = do
    putStrLn $ "¿Dónde quieres conectar el nodo " ++ show nodo ++ "? (separados por espacios)"
    entradaConexiones <- getLine
    let conexiones = map read (words entradaConexiones) :: [Int]
    let conexionesValidas = filter (\x -> x /= nodo && x >= 1 && x <= n) conexiones
    let conexionesUnicas = nub conexionesValidas
    let nuevasConexiones = filter (`notElem` (grafo !! (nodo - 1))) conexionesUnicas

    if length nuevasConexiones /= length conexionesUnicas
        then do
            putStrLn "Algunas conexiones ya existen, intenta de nuevo."
            conectarNodos n grafo nodo
        else do
            let grafoActualizado = agregarConexiones grafo nodo nuevasConexiones
            return grafoActualizado

agregarConexiones :: Grafo -> Int -> [Int] -> Grafo
agregarConexiones grafo nodo conexiones =
    let filaActualizada = nub (grafo !! (nodo - 1) ++ conexiones)
        nuevoGrafo = take (nodo - 1) grafo ++ [filaActualizada] ++ drop nodo grafo
    in foldl (\g conn -> agregarConexion g conn nodo) nuevoGrafo conexiones

agregarConexion :: Grafo -> Int -> Int -> Grafo
agregarConexion grafo desde hasta =
    let filaDesde = nub (grafo !! (desde - 1) ++ [hasta])
        filaHasta = nub (grafo !! (hasta - 1) ++ [desde])
        nuevoGrafo = take (desde - 1) grafo ++ [filaDesde] ++ drop desde grafo
    in take (hasta - 1) nuevoGrafo ++ [filaHasta] ++ drop hasta nuevoGrafo

imprimirGrafo :: Grafo -> IO ()
imprimirGrafo grafo = do
    putStrLn "CONEXIONES DEL GRAFO CON USO DE CURRIFICACION---"
    mapM_ imprimirConexiones (zip [1..] grafo)

imprimirConexiones :: (Int, [Int]) -> IO ()
imprimirConexiones (nodo, conexiones) =
    putStrLn $ show nodo ++ " -> " ++ (if null conexiones then "" else unwords (map show conexiones))
