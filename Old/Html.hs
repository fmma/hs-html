module Html where

data Page where
    Page :: Model a -> View a -> Page

data Model a where
    Empty :: Model ()
    Decl :: Decl a -> Model a
    Pair :: Model a -> Model b -> Model (a, b)

data Decl a where
    DInt :: Decl Int

data View a where
    Div :: [View a] -> View a
    Input :: (Int -> a -> a) -> (a -> Int) -> View a
    Text :: (a -> String) -> View a
    Button :: (a -> a) -> (a -> String) -> View a

instance Monoid Page where
    mempty = Page Empty (Div [])
    mappend (Page m1 v1) (Page m2 v2) = Page (Pair m1 m2) (Div [weakenView (\ f (x, y) -> (f x, y)) fst v1, weakenView (\ f (x, y) -> (x, f y)) snd v2])

decl :: Decl a -> Page -> Page
decl d (Page model view) = Page (Pair (Decl d) model) (weakenView (\ f (x, y) -> (x, f y)) snd view)

weakenView :: ((a -> a) -> b -> b) -> (b -> a) -> View a -> View b
weakenView wm w v =
    case v of
        Div cs -> Div (map (weakenView wm w) cs)
        Input f g -> Input (\x -> wm (f x)) (g . w)
        Text g -> Text (g . w)
        Button f g -> Button (wm f) (g . w)
