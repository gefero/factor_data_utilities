
# Citar a Kate Lyons + Jessica Peterka-Bonetta por diccionario de emojis
dic_emojis <- read_csv("./data/raw/dic_emojis.csv") %>% 
        # espacios al principio y al final porque los emojis suelen escibirse pegados
        mutate(EN = paste0(" emoji_", str_replace_all(EN, " ", "_"), " ")) %>% 
        arrange(desc(utf8))

#Para eliminar tildes, diéresis, etc
removeDiacritics <- function(string) {
        chartr(
                "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðòóôõöùúûüýÿ",
                "SZszYAAAAAACEEEEIIIIDOOOOOUUUUYaaaaaaceeeeiiiidooooouuuuyy",
                string
        )
}

limpiar_texto_tweet <- function(text, emoji_from, emoji_to) {
        # Util para eliminar links luego
        url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
        # y puntuacion y cosos, pero no hashtags
        simbolos_molestos <- "[!\\?:\\-\\+\\.,;><\\|]" 
        text %>% 
                #eliminar tildes y diéresis
                removeDiacritics() %>% 
                # pasar encoding a ASCII para poder matchear 
                # contra diccionario de emojis
                iconv(from="UTF-8", to ="ascii", sub = "byte") %>% 
                # recuperar las "ñ", que se pierden con el paso a ascii
                str_replace_all("<c3><b1>", "ñ") %>% 
                # reemplazar código de emojis por descripcion, per diccionario
                stringi::stri_replace_all_fixed(emoji_from, emoji_to,
                                                vectorize_all = FALSE) %>%
                # eliminar "mentions" (arrobas)
                str_remove_all("[@][\\w_-]+") %>% 
                # cambiar URLs por etiqueta genérica
                str_replace_all(url_pattern, "_url_") %>% 
                # eliminar ">", "<" y "/" tal como aparecen en el encoding
                str_remove_all("&gt;|&lt;|&amp;") %>% 
                # eliminar resaca unicode (comillas izquierda y derecha, 
                # espacios raros, signos de exclamacion invertidos, etc)
                str_remove_all("<.*>") %>%
                # eliminar puntuacion y cosos, pero no hashtags
                str_replace_all(simbolos_molestos, " ") %>% 
                # cambiar digitos por etiqueta genérica
                str_replace_all("(?<=^|\\s)\\d+", "_digitos_") %>% 
                # a minuscula
                tolower() %>% 
                # colapsar multiples espacios en blanco seguidos en uno solo
                str_squish() 
}
