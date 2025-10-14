DATABASE safre_af
GLOBALS

    DEFINE 
        llama_pgm  CHAR(500),
        enter      CHAR(1)

    DEFINE
        valor1     SMALLINT
END GLOBALS
MAIN
    LET llama_pgm = " fglgo AFIM001 "," ",51
    RUN llama_pgm
END MAIN
