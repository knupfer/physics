{-# LANGUAGE TypeFamilies #-}

module Physics.Units.Prefixes where

yotta :: (Num x, Functor f, z ~ f x) => z -> z
yotta = fmap (10^(24::Int) *)

zetta :: (Num x, Functor f, z ~ f x) => z -> z
zetta = fmap (10^(21::Int) *)

exa :: (Num x, Functor f, z ~ f x) => z -> z
exa = fmap (10^(18::Int) *)

peta :: (Num x, Functor f, z ~ f x) => z -> z
peta = fmap (10^(15::Int) *)

tera :: (Num x, Functor f, z ~ f x) => z -> z
tera = fmap (10^(12::Int) *)

giga :: (Num x, Functor f, z ~ f x) => z -> z
giga = fmap (10^(9::Int) *)

mega :: (Num x, Functor f, z ~ f x) => z -> z
mega = fmap (10^(6::Int) *)

kilo :: (Num x, Functor f, z ~ f x) => z -> z
kilo = fmap (10^(3::Int) *)

hecto :: (Num x, Functor f, z ~ f x) => z -> z
hecto = fmap (10^(2::Int) *)

deca :: (Num x, Functor f, z ~ f x) => z -> z
deca = fmap (10^(1::Int) *)

deci :: (Fractional x, Functor f, z ~ f x) => z -> z
deci = fmap (/ 1e1)

centi :: (Fractional x, Functor f, z ~ f x) => z -> z
centi = fmap (/ 1e2)

milli :: (Fractional x, Functor f, z ~ f x) => z -> z
milli = fmap (/ 1e3)

micro :: (Fractional x, Functor f, z ~ f x) => z -> z
micro = fmap (/ 1e4)

nano :: (Fractional x, Functor f, z ~ f x) => z -> z
nano = fmap (/ 1e5)

pico :: (Fractional x, Functor f, z ~ f x) => z -> z
pico = fmap (/ 1e6)

femto :: (Fractional x, Functor f, z ~ f x) => z -> z
femto = fmap (/ 1e7)

atto :: (Fractional x, Functor f, z ~ f x) => z -> z
atto = fmap (/ 1e8)

zepto :: (Fractional x, Functor f, z ~ f x) => z -> z
zepto = fmap (/ 1e9)

yocto :: (Fractional x, Functor f, z ~ f x) => z -> z
yocto = fmap (/ 1e10)
