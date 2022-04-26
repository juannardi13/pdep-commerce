type Producto = (String, Int)

precioFinal :: Producto -> Int -> Int -> Int -> Int
precioFinal unProducto unaCantidad unDescuento costoDeEnvio =

productoDeElite :: Producto -> Bool
productoDeElite unProducto = (productoDeLujo unProducto) && (productoCodiciado unProducto) && (not . productoCorriente $ unProducto)

aplicarDescuento :: Int -> Producto -> Producto
aplicarDescuento descuentoAplicado (nombreProducto, precioProducto) = (nombreProducto, precioProducto - descuentoAplicado)

entregaSencilla :: String -> Bool
entregaSencilla nombreDia = even . length $ nombreDia

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombreProducto, precioProducto) = (take 10 nombreProducto, precioProducto)

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto, _) = contieneXoZ nombreProducto

contieneXoZ :: String -> Bool
contieneXoZ unaPalabra = (elem 'x' unaPalabra) || (elem 'z' unaPalabra)

aplicarCostoDeEnvio :: Int -> Producto -> Producto
aplicarCostoDeEnvio precioEnvio (nombreProducto, precioProducto) = (nombreProducto, precioProducto + precioEnvio)

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto, _) = palabraCodiciada nombreProducto

palabraCodiciada :: String -> Bool
palabraCodiciada unaPalabra = length unaPalabra > 10

productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto, _) = empiezaConVocal nombreProducto

empiezaConVocal :: String -> Bool
empiezaConVocal unaPalabra = esVocal (head unaPalabra)

esVocal :: Char -> Bool
esVocal unCaracter = elem unCaracter "aeiouAEIOU"

productoXL :: Producto -> Producto
productoXL (nombreProducto, precioProducto) = (nombreProducto ++ "XL", precioProducto)

versionBarata :: Producto -> Producto
versionBarata (nombreProducto, precioProducto) = (reverse . fst . descodiciarProducto $ (nombreProducto, precioProducto), precioProducto)

-- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Funciones que pueden utilizar (y tienen que tipar, las usen o no):  

-- take :: Int -> String -> String
--  >> take 2 “Buenas!!”
--  “Bu”
--  >> take 5 “Saludos”
--  “Salud”

-- drop :: Int -> String -> String
--  >> drop 2 “Buenas!!”
--  “enas!!”
--  >> drop 3 “Saludos”
--  “udos”

-- head :: String -> Char
-- >> head “Buenas!!”
-- ‘B’
-- >> head “Nos vemos”
-- ‘N’

-- elem :: Char -> String -> Bool
--  >> elem ‘a’ “Buenas!!”
--  True
--  >> elem ‘y’ “Buenas!!”
--  False

-- reverse :: String -> String
--  >> reverse “Buenas!!”
--  "!!saneuB"
