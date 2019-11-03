let disemvowel (s:string) = 
    Array.reduce (+) (s.Split([|'a';'e';'i';'o';'u';'A';'E';'I';'O';'U'|]))
