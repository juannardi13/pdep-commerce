type Producto = (String, Float)

precioFinal :: Producto -> Float -> Float -> Float -> Float
precioFinal unProducto unaCantidad unDescuento costoDeEnvio = (precioDelProducto) . (aplicarCostoDeEnvio costoDeEnvio) . (precioPorCantidad unaCantidad) . (aplicarDescuento unDescuento) $ unProducto

precioPorCantidad :: Float -> Producto -> Producto -- funcion para tratar de dar más expresividad al código
precioPorCantidad unaCantidad (nombreProducto, precioProducto) = (nombreProducto, unaCantidad * precioProducto)

-- me hacía ruido que una cantidad de un producto sea flotante y no entera, pero pensé que quizás se pueden comprar productos por peso (fiambres tal vez) y el precio en la dupla sea el precio por kilo.
-- así no tendría problemas de tipos con ninguna función :D

precioDelProducto :: Producto -> Float -- funcion para tratar de dar más expresividad al código
precioDelProducto unProducto = snd unProducto

productoDeElite :: Producto -> Bool
productoDeElite unProducto = (productoDeLujo unProducto) && (productoCodiciado unProducto) && (not . productoCorriente $ unProducto)

aplicarDescuento :: Float -> Producto -> Producto
aplicarDescuento descuentoAplicado (nombreProducto, precioProducto) = (nombreProducto, precioProducto - (precioProducto * (descuentoAplicado / 100)))

entregaSencilla :: String -> Bool
entregaSencilla nombreDia = even . length $ nombreDia

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombreProducto, precioProducto) = (take 10 nombreProducto, precioProducto)

productoDeLujo :: Producto -> Bool 
productoDeLujo (nombreProducto, _) = contieneXoZ nombreProducto

contieneXoZ :: String -> Bool -- funcion para tratar de dar más expresividad al código
contieneXoZ unaPalabra = (elem 'x' unaPalabra) || (elem 'z' unaPalabra)

aplicarCostoDeEnvio :: Float -> Producto -> Producto
aplicarCostoDeEnvio precioEnvio (nombreProducto, precioProducto) = (nombreProducto, precioProducto + precioEnvio)

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto, _) = palabraCodiciada nombreProducto

palabraCodiciada :: String -> Bool -- funcion para tratar de dar más expresividad al código
palabraCodiciada unaPalabra = length unaPalabra > 10

productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto, _) = empiezaConVocal nombreProducto

empiezaConVocal :: String -> Bool -- funcion para tratar de dar más expresividad al código
empiezaConVocal unaPalabra = esVocal (head unaPalabra)

esVocal :: Char -> Bool -- funcion para tratar de dar más expresividad al código
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
