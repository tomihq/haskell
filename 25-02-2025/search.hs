data FS = File String Int | Folder String [FS] deriving Show

foldFS fFile fFolder tree = case tree of
    File name size -> fFile name size
    Folder name children -> fFolder name (map (foldFS fFile fFolder) children)

findFile :: FS -> String -> Maybe String
findFile = foldFS
    (\ name size query -> if name == query then Just name else Nothing)
    (\ name resHijos query -> foldr (\hijo acc ->
            case hijo query of
                Just encontrado -> Just encontrado
                Nothing         -> acc
        ) Nothing resHijos)

miArbol = Folder "Root" [File "nota.txt" 1, Folder "Docs" [File "tesis.pdf" 10]]