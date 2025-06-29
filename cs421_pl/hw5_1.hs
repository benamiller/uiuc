maxk a b k =
    if a > b
        then k a
        else k b

max3k a b c k =
    maxk b c (\max_b_c ->
        maxk a max_b_c (\max_overall ->
            k (9 + max_overall)
        )
    )
