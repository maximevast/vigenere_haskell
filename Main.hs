import Utils

-- return the corresponding char at index 'z % alphabet_lenght'
encryption_convert :: Int -> Char
encryption_convert z = at_index (z `mod` alphabet_lenght)

-- return the corresponding char at index -- 'z' if 'z > 0'
--                                        -- 'z + alphabet_lenght' otherwise
decryption_convert :: Int -> Char
decryption_convert z
  | z < 0 = at_index (z + alphabet_lenght)
  | otherwise = at_index (z)

-- return the key in UPERCASE, without spaces and repeated until xs.length
format_key :: [Char] -> [Char] -> [Char]
format_key xs ys = extend_key xs (to_upper_case (remove_spaces ys) )

-- encrypt a string given a key
vigenere_encrypt :: [Char] -> [Char] -> [Char]
vigenere_encrypt xs ys
  | not(is_valid(plain_text))   = error "Invalid plain text"
  | not(is_valid(key))          = error "Invalid key"
  | length(xs)<=length(ys)      = error "Key is too long"
  | otherwise                   = map encryption_convert matches
  where
    plain_text = to_upper_case (remove_spaces xs)
    key = format_key plain_text ys
    -- create a new int list suming indexes of plain_text and key
    matches =  zipWith (+) (map index_of (plain_text)) (map index_of (key))

-- decrypt a string given a key
vigenere_decrypt :: [Char] -> [Char] -> [Char]
vigenere_decrypt xs ys
  | not(is_valid(cipher_text))  = error "Invalid cipher text"
  | not(is_valid(key))          = error "Invalid key"
  | length(xs)<=length(ys)      = error "Key is too long"
  | otherwise                   = map decryption_convert matches
  where
    cipher_text = to_upper_case (remove_spaces xs)
    key = format_key cipher_text ys
    -- create a new int list substracting indexes of cipher_text and key
    matches =  zipWith (-) (map index_of (cipher_text)) (map index_of (key))
