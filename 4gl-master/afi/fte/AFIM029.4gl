    DEFINE
        sw_1     ,
        digito   ,
        contador SMALLINT

    DEFINE
        HOY     DATE

    DEFINE
        vubicacion LIKE afi_mae_afiliado.ubicacion

    DEFINE gmaster ARRAY [5000] OF RECORD
        indicador_b LIKE afi_mae_afiliado.indicador_b ,
        n_folio     LIKE afi_mae_afiliado.n_folio     ,
        n_seguro    LIKE afi_mae_afiliado.n_seguro    ,
        npm         CHAR(80)
    END RECORD

    DEFINE
        g_hora CHAR(8),
        pat    CHAR(30),
        mat    CHAR(30),
        nom    CHAR(30)

	OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0291" ATTRIBUTE(BORDER)
	DISPLAY " AFIM029                 CONSULTA POR CAJA                                          " AT 3,1 ATTRIBUTE(REVERSE)
	
	MENU "PRE-AFILIADOS"
              COMMAND "Consulta" "Consulta de Pre-Afiliados"
                      CALL Consulta()
	              #CALL Inicializa()
              COMMAND "Salir" "Salir de Programa"
		      EXIT MENU
	END MENU

