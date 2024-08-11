module Chapter08.Part04.Tree4 where

data Tree a = Node a [Tree a]

tree :: Tree String
tree =
    Node
        "Music"
        [ Node
            "Well known"
            [ Node
                "Guns and Roses"
                [ Node "Sweet Child of Mine" []
                , Node "Welcome to the Jungle" []
                ]
            , Node
                "Metallica"
                [ Node "Master Of Puppets" []
                , Node "For Whom The Bell Tolls" []
                ]
            ]
        , Node
            "Not well known"
            [ Node
                "Textures"
                [ Node "Reaching Home" []
                , Node "Foreclosure" []
                ]
            , Node
                "Anathema"
                [ Node "Untouchable Part 1" []
                , Node "Untouchable Part 1" []
                ]
            , Node
                "Lazuli"
                [ Node "Dans Les Mains De Dieter" []
                , Node "Baume" []
                , Node "Un Visage Lunaire" []
                ]
            ]
        ]
