#############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                          #
#Propietario       => E.F.P.                                                #
#Programa AFIL030  => ESTADOS DEL AFILIADO (APROBADOS)                      #
#Fecha             => 2 Junio 1997.      				    #
#Por               => JUAN COLIN M.                                         #
#Modificado por    => FERNANDO HERRERA HERNANDEZ.                           #
#Fecha modificacion=> 5 Agosto 2003.                                        #
#Sistema           => AFI. Version 2                                        #
#############################################################################
DATABASE safre_af
GLOBALS
   DEFINE g_reg       RECORD
          fecha_ini   DATE,
          fecha_top   DATE,
          estruc      CHAR(5),
          promotor    CHAR(12),
          ord         SMALLINT,
          opcion      SMALLINT
   END RECORD

   DEFINE w_aux       RECORD
          lote        LIKE afi_solicitud.lote,
          agenc_cod   LIKE afi_solicitud.agenc_cod,
          #nro_solic   LIKE pro_mae_promotor.nro_solicitud,
          nro_solic   DECIMAL(10,0),
          ppaterno    LIKE pro_mae_promotor.paterno,
          mmaterno    LIKE pro_mae_promotor.materno,
          nnombres    LIKE pro_mae_promotor.nombres,
          n_folio     LIKE afi_solicitud.n_folio,
          fentcons    LIKE afi_solicitud.fentcons,
          paterno     LIKE afi_solicitud.paterno,
          materno     LIKE afi_solicitud.materno,
          nombres     LIKE afi_solicitud.nombres,
          n_seguro    LIKE afi_solicitud.n_seguro,
          n_unico     LIKE afi_solicitud.n_unico,
          usuario     LIKE afi_solicitud.usuario
   END RECORD

   DEFINE hoy         DATE
   DEFINE hora        CHAR(05)
   DEFINE estrc       CHAR(16)
   DEFINE aux_pausa   CHAR(01)
   DEFINE l_estado    CHAR(16)
   DEFINE g_afore     RECORD LIKE tab_afore.*

   DEFINE g_usuario   CHAR(08)
   DEFINE tipo_sol    CHAR(18)
   DEFINE G_LISTA     CHAR(100)
   DEFINE txt1        CHAR(800)
   DEFINE vimpresion  CHAR(100)

   DEFINE g_paramgrales  RECORD LIKE seg_modulo.*

END GLOBALS

MAIN
   OPTIONS 
   PROMPT LINE LAST,
   ACCEPT KEY CONTROL-I,
   FORM LINE 3

   CALL STARTLOG("AFIL030.log")
   LET hora = TIME
   LET hoy = TODAY

   OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIL0301" ATTRIBUTE(BORDER)
   DISPLAY "AFIL030   LISTADO DE AFILIADOS ACEPTADOS (REGISTRO o TRASPASO)             " AT 3,1 ATTRIBUTE(REVERSE)

   DISPLAY "                           < Ctrl-C > Salir                                " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING "dd-mm-yyyy" AT 3,66 ATTRIBUTE(REVERSE)

   LET g_reg.fecha_ini = TODAY
   LET g_reg.fecha_top = TODAY
   LET g_reg.estruc = NULL
   LET g_reg.promotor = NULL
   LET g_reg.ord = 1

   INPUT BY NAME g_reg.* WITHOUT DEFAULTS 
     AFTER FIELD fecha_ini
     IF g_reg.fecha_ini IS NULL THEN
        ERROR "LA FECHA INICIAL NO PUEDE SER NULA"
        NEXT FIELD  fecha_ini
     END IF

     AFTER FIELD fecha_top
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        NEXT FIELD fecha_ini
     END IF

     IF g_reg.fecha_top IS NULL THEN
        ERROR "LA FECHA FINAL NO PUEDE SER NULA"
        NEXT FIELD  fecha_top
     ELSE
        NEXT FIELD estruc
     END IF

     IF g_reg.fecha_top < g_reg.fecha_ini THEN
        ERROR "FECHA NO PUEDE SER MENOR A LA FECHA INICIAL"
        NEXT FIELD fecha_top
     END IF

     BEFORE FIELD estruc
     LET g_reg.estruc = "TODOS"

     AFTER FIELD estruc   
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        LET g_reg.promotor = NULL
        DISPLAY g_reg.promotor
        NEXT FIELD fecha_ini
     END IF

     IF g_reg.estruc IS NULL THEN
        NEXT FIELD promotor
     ELSE 
        NEXT FIELD ord
     END IF

     BEFORE FIELD promotor
     LET g_reg.promotor = "TODOS"

     AFTER FIELD promotor
     IF fgl_lastkey() = fgl_keyval("UP") THEN
        LET g_reg.promotor = NULL
        LET g_reg.estruc = NULL
        DISPLAY g_reg.estruc
        DISPLAY g_reg.promotor
        NEXT FIELD estruc
     END IF

     IF g_reg.promotor IS NULL THEN
        NEXT FIELD estruc
     ELSE 
        NEXT FIELD ord
     END IF

     AFTER FIELD ord
     IF g_reg.ord IS NULL THEN
        ERROR "EL CRITERIO DE ORDENAMIENTO NO PUEDE SER NULO"
        NEXT FIELD ord
     END IF

     IF g_reg.ord > 3 OR g_reg.ord = 0 THEN
        NEXT FIELD ord
     END IF

     AFTER FIELD opcion
     IF g_reg.opcion IS NULL THEN
        ERROR "LA OPCION NO PUEDE SER NULO" 
        NEXT FIELD opcion
     END IF

     IF g_reg.opcion > 2 OR g_reg.opcion = 0 THEN
        ERROR "LA OPCION SOLO PUEDE SER 1 o 2"
        NEXT FIELD opcion
     END IF

   END INPUT

   WHILE TRUE
     PROMPT "DESEA EMITIR INFORME S/N ==> " FOR aux_pausa
     IF aux_pausa MATCHES "[SsNn]" THEN
        EXIT WHILE
     END IF
   END WHILE

   IF aux_pausa MATCHES "[Nn]" THEN
      ERROR "PROCESO CANCELADO " SLEEP 2
   ELSE
      ERROR "PROCESANDO INFORMACION ... ESPERE UN MOMENTO"

      SELECT *, USER 
      INTO   g_afore.*, g_usuario 
      FROM   tab_afore
      WHERE  @marca = 1

      SELECT *
      INTO g_paramgrales.*
      FROM seg_modulo    
      WHERE modulo_cod = 'afi'

      IF g_reg.opcion = 1 THEN
         LET tipo_sol = ".ACEP_REG."
      ELSE
         LET tipo_sol = ".ACEP_TRA."
      END IF
      
      LET G_LISTA = g_paramgrales.ruta_listados CLIPPED,"/",g_usuario CLIPPED,
                    tipo_sol CLIPPED,HOY USING "DDMMYY","_",hora[1,2],hora[4,5] CLIPPED


   CASE g_reg.ord
        WHEN 1
        START REPORT listado1 TO G_LISTA

        LET txt1 = " SELECT ",
                   " a.lote          ,",
                   " a.agenc_cod     ,",
                   " b.nro_solicitud ,",
                   " b.paterno       ,",
                   " b.materno       ,",
                   " b.nombres       ,",
                   " a.n_folio       ,",
                   " a.fentcons      ,",
                   " a.paterno       ,",
                   " a.materno       ,",
                   " a.nombres       ,",
                   " a.n_seguro      ,",
                   " a.n_unico       ,",
                   " a.usuario        ",
                   " FROM afi_solicitud a, pro_mae_promotor b",
                   " WHERE a.status_interno in (60,65,70,75,100) ",
                   " AND a.tipo_solicitud = '",g_reg.opcion,"'",
                   " AND a.fentcons BETWEEN '",g_reg.fecha_ini,"'",
                   " AND '",g_reg.fecha_top,"'" CLIPPED

        IF g_reg.estruc IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," AND a.cod_promotor = b.cod_promotor"
           IF g_reg.estruc != "TODOS" THEN
              LET txt1 = txt1 CLIPPED,
              " AND a.agenc_cod = '",g_reg.estruc,"'",
              " ORDER BY a.lote,a.n_seguro"
           ELSE
              LET txt1 = txt1 CLIPPED," ORDER BY a.lote,b.nro_solicitud "
           END IF
        END IF

        IF g_reg.promotor IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," AND a.cod_promotor = b.cod_promotor"
           IF g_reg.promotor != "TODOS" THEN
              LET txt1 = txt1 CLIPPED,
              " AND a.nro_solicitud = '",g_reg.promotor,"'",
              " ORDER BY a.lote,a.n_seguro,a.agenc_cod"
           ELSE
              LET txt1 = txt1 CLIPPED,
              " ORDER BY a.lote,a.n_seguro,a.agenc_cod"
           END IF
        END IF

       #DISPLAY txt1 ; exit program
        PREPARE hhh FROM txt1
        DECLARE cur_1 CURSOR for hhh
          FOREACH cur_1 INTO w_aux.*
            OUTPUT TO REPORT listado1(w_aux.*)       
          END FOREACH 

        FINISH REPORT listado1
        LET vimpresion = "lp ", G_LISTA 
        RUN vimpresion
        PROMPT "LISTADO GENERADO ==> ",G_LISTA CLIPPED,"  <enter>" FOR aux_pausa

        WHEN 2
        START REPORT listado2 TO G_LISTA

        LET txt1 = " SELECT ",
                   " a.lote      ,",
                   " a.agenc_cod ,",
                   " b.cod_promotor    ,",
                   " b.paterno   ,",
                   " b.materno   ,",
                   " b.nombres   ,",
                   " a.n_folio   ,",
                   " a.fentcons  ,",
                   " a.paterno   ,",
                   " a.materno   ,",
                   " a.nombres   ,",
                   " a.n_seguro  ,",
                   " a.n_unico   ,",
                   " a.usuario         ",
                   " FROM afi_solicitud a, pro_mae_promotor b ",
                   " WHERE  a.status_interno in (60,65,70,75,100)  ",
                   " AND a.tipo_solicitud = '",g_reg.opcion,"'",
                   " AND a.fentcons BETWEEN '",g_reg.fecha_ini,"'",
                   " AND '",g_reg.fecha_top,"'" CLIPPED

        IF g_reg.estruc IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," AND a.cod_promotor = b.cod_promotor"
           IF g_reg.estruc != "TODOS" THEN
              LET txt1 = txt1 CLIPPED,
              " AND a.agenc_cod = '",g_reg.estruc,"'",
              " ORDER BY a.lote,a.n_seguro,b.nro_solicitud "
           ELSE
              LET txt1 = txt1," ORDER BY a.lote,a.n_seguro,b.nro_solicitud"
           END IF
        END IF

        IF g_reg.promotor IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," AND a.cod_promotor = b.cod_promotor"
           IF g_reg.promotor != "TODOS" THEN
              LET txt1 = txt1 CLIPPED,
              " AND a.nro_solicitud = '",g_reg.promotor,"'",
              " ORDER BY  a.lote,a.n_seguro,b.cod_promotor "
           ELSE
              LET txt1 = txt1 CLIPPED,
              " ORDER BY a.lote,a.n_seguro,c.cod_promotor"
           END IF
        END IF

        PREPARE hhh1 FROM txt1
        DECLARE cur_2 CURSOR for hhh1
          FOREACH cur_2 INTO w_aux.*
            OUTPUT TO REPORT listado2(w_aux.*)       
          END FOREACH 

        FINISH REPORT listado2
        LET vimpresion = "lp ", G_LISTA 
        RUN vimpresion
        PROMPT "LISTADO GENERADO ==> ",G_LISTA CLIPPED,"  <enter>" FOR aux_pausa

        WHEN 3
        START REPORT listado3 TO G_LISTA

        LET txt1 = " SELECT ",
                   " a.lote      ,",
                   " a.agenc_cod ,",
                   " b.cod_promotor    ,",
                   " b.paterno   ,",
                   " b.materno   ,",
                   " b.nombres   ,",
                   " a.n_folio   ,",
                   " a.fentcons  ,",
                   " a.paterno   ,",
                   " a.materno   ,",
                   " a.nombres   ,",
                   " a.n_seguro  ,",
                   " a.n_unico   ,",
                   " a.usuario    ",
                   " FROM afi_solicitud a, pro_mae_promotor b ",
                   " WHERE a.status_interno in (60,65,70,75,100) ",
                   " AND a.tipo_solicitud = '",g_reg.opcion,"'",
                   " AND a.fentcons BETWEEN '",g_reg.fecha_ini,"'", 
                   " AND '",g_reg.fecha_top,"'" CLIPPED

        IF g_reg.estruc IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," AND a.cod_promotor = b.cod_promotor"
           IF g_reg.estruc != "TODOS" THEN
              LET txt1 = txt1 CLIPPED,
              " AND a.agenc_cod = '",g_reg.estruc,"'",
              " ORDER BY a.lote,a.n_seguro,a.fentcons "
           ELSE
              LET txt1 = txt1," ORDER BY a.lote,a.n_seguro,a.fentcons "
           END IF
        END IF

        IF g_reg.promotor IS NOT NULL THEN
           LET txt1 = txt1 CLIPPED," AND   a.cod_promotor = b.cod_promotor"
           IF g_reg.promotor != "TODOS" THEN
              LET txt1 = txt1 CLIPPED,
              " AND b.nro_solicitud = '",g_reg.promotor,"'",
              " ORDER BY  a.lote,a.n_seguro,a.fentcons "
           ELSE
              LET txt1 = txt1 CLIPPED,
              " ORDER BY  a.lote,a.n_seguro,a.fentcons "
           END IF
        END IF

        PREPARE hhh2 FROM txt1
        DECLARE cur_3 CURSOR for hhh2
          FOREACH cur_3 INTO w_aux.*
            OUTPUT TO REPORT listado3(w_aux.*)       
          END FOREACH 

        FINISH REPORT listado3
        LET vimpresion = "lp ", G_LISTA 
        RUN vimpresion
        PROMPT "LISTADO GENERADO ==> ",G_LISTA CLIPPED,"  <enter>" FOR aux_pausa

        OTHERWISE 
        ERROR "OPCION ERRONEA ... FAVOR DE VERIFICAR" SLEEP 2
   END CASE

   END IF

   EXIT PROGRAM 

END MAIN


REPORT listado1(w_aux)
#l1-------------------
   DEFINE w_aux       RECORD
          lote        LIKE afi_solicitud.lote,
          agenc_cod   LIKE afi_solicitud.agenc_cod,
          nro_solic   LIKE pro_mae_promotor.nro_solicitud ,
          ppaterno    LIKE pro_mae_promotor.paterno,
          mmaterno    LIKE pro_mae_promotor.materno,
          nnombres    LIKE pro_mae_promotor.nombres,
          n_folio     LIKE afi_solicitud.n_folio,
          fentcons    LIKE afi_solicitud.fentcons,
          paterno     LIKE afi_solicitud.paterno ,
          materno     LIKE afi_solicitud.materno ,
          nombres     LIKE afi_solicitud.nombres ,
          n_seguro    LIKE afi_solicitud.n_seguro,
          n_unico     LIKE afi_solicitud.n_unico,
          usuario     LIKE afi_solicitud.usuario
   END RECORD

   DEFINE razon_social    CHAR(40)

   OUTPUT
   PAGE LENGTH 90
   LEFT MARGIN 0
   RIGHT MARGIN 0
   TOP MARGIN 0
   BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

     PRINT
     COLUMN 001,razon_social,
     COLUMN 140,"FECHA   : ",hoy USING "DD/MM/YYYY ",
     COLUMN 149,hora [1,5]

     IF g_reg.opcion = "1" THEN
        PRINT
        COLUMN 001,"AFIL030", 
        COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R    R E G I S T R O    A P R O B A D A S",   
        COLUMN 140,"P\240gina : ",pageno USING "#####"
     ELSE
        PRINT
        COLUMN 001,"AFIL030", 
        COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R    T R A S P A S O    A P R O B A D A S",   
        COLUMN 140,"P\240gina : ",pageno USING "#####"
     END IF
      
     PRINT
     PRINT
     COLUMN 67,"Del : ",g_reg.fecha_ini USING "DD/MM/YYYY",
               " Al : ",g_reg.fecha_top USING "DD/MM/YYYY" 

     PRINT
     COLUMN 001,"----------------------------------------",
     COLUMN 040,"----------------------------------------",
     COLUMN 080,"----------------------------------------",
     COLUMN 120,"----------------------------------------",
     COLUMN 160,"---------"

     PRINT
     COLUMN 001,"Lote",
     COLUMN 006,"Estr",
     COLUMN 013,"N.I.Promo.",
     COLUMN 026,"Nombre Promotor",
     COLUMN 058,"Folio",
     COLUMN 070,"F. aproba.",
     COLUMN 084,"Nombre",
     COLUMN 118,"N.S.S.",
     COLUMN 132,"C.U.R.P",
     COLUMN 146,"USUARIO"            

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1
     INTO   estrc 
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     PRINT
     COLUMN 01,w_aux.lote USING "##",
     COLUMN 07,estrc [1,6] ,
     COLUMN 13,w_aux.nro_solic USING "###########",
     COLUMN 26,w_aux.ppaterno CLIPPED," " ,w_aux.nnombres [1,13] ,
     COLUMN 58,w_aux.n_folio  USING "&&&&&&&&",
     COLUMN 70,w_aux.fentcons USING  "DD/MM/YYYY", 
     COLUMN 84,w_aux.paterno CLIPPED," ",w_aux.materno CLIPPED
              ," ",w_aux.nombres [1,9],
     COLUMN 118,w_aux.n_seguro USING "&&&&&&&&&&&",
     COLUMN 132,w_aux.n_unico, 
     COLUMN 146,w_aux.usuario 

   AFTER GROUP OF w_aux.lote

     PRINT
     PRINT
     COLUMN 01,"SOLICITUDES APROBADAS EN LOTE ",
                w_aux.lote USING "###"," : ",GROUP COUNT(*) USING "&&&&&&"

     SKIP TO TOP OF PAGE

   ON LAST ROW

     PRINT 
     COLUMN 01,"TOTAL DE SOLICITUDES           :  ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT


REPORT listado2(w_aux)
#l2-------------------
   DEFINE w_aux       RECORD
          lote        LIKE afi_solicitud.lote,
          agenc_cod   LIKE afi_solicitud.agenc_cod,
          nro_solic   LIKE pro_mae_promotor.nro_solicitud ,
          ppaterno    LIKE pro_mae_promotor.paterno,
          mmaterno    LIKE pro_mae_promotor.materno,
          nnombres    LIKE pro_mae_promotor.nombres,
          n_folio     LIKE afi_solicitud.n_folio,
          fentcons    LIKE afi_solicitud.fentcons,
          paterno     LIKE afi_solicitud.paterno ,
          materno     LIKE afi_solicitud.materno ,
          nombres     LIKE afi_solicitud.nombres ,
          n_seguro    LIKE afi_solicitud.n_seguro,
          n_unico     LIKE afi_solicitud.n_unico,
          usuario     LIKE afi_solicitud.usuario
   END RECORD

   DEFINE razon_social    CHAR(40)

   OUTPUT
   PAGE LENGTH 90
   LEFT MARGIN 0
   RIGHT MARGIN 0
   TOP MARGIN 0
   BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

     PRINT
     COLUMN 001,razon_social,
     COLUMN 140,"FECHA   :",hoy USING "DD/MM/YYYY ", 
     COLUMN 149,hora [1,5]

     IF g_reg.opcion = "1" THEN
        PRINT
        COLUMN 001,"AFIL030", 
        COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R    R E G I S T R O    A P R O B A D A S",   
        COLUMN 140,"P\240gina : ",pageno USING "#####"
     ELSE
        PRINT
        COLUMN 001,"AFIL030", 
        COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R    T R A S P A S O    A P R O B A D A S",   
        COLUMN 140,"P\240gina : ",pageno USING "#####"
     END IF
      
     PRINT
     PRINT
     COLUMN 67,"Del : ",g_reg.fecha_ini USING "DD/MM/YYYY",
               " Al : ",g_reg.fecha_top USING "DD/MM/YYYY" 

     PRINT
     COLUMN 001,"----------------------------------------",
     COLUMN 040,"----------------------------------------",
     COLUMN 080,"----------------------------------------",
     COLUMN 120,"----------------------------------------",
     COLUMN 160,"---------"

     PRINT
     COLUMN 01,"Lote",
     COLUMN 07,"Estr",
     COLUMN 13,"N. Promo.", 
     COLUMN 26,"Nombre Promotor", 
     COLUMN 50,"Folio",
     COLUMN 76,"F. aproba.", 
     COLUMN 88, "Nombre",
     COLUMN 118,"N.S.S.",
     COLUMN 132,"C.U.R.P",
     COLUMN 144,"USUARIO"            

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

   ON EVERY ROW

     LET estrc = NULL

     SELECT nombre_uni_n1 
     INTO   estrc 
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     PRINT
     COLUMN 01,w_aux.lote USING "##" ,
     COLUMN 07,estrc [1,6] ,
     COLUMN 13,w_aux.nro_solic,
     COLUMN 26,w_aux.ppaterno CLIPPED," ",w_aux.nnombres [1,13] ,
     COLUMN 50,w_aux.n_folio  USING "&&&&&&&&",
     COLUMN 76,w_aux.fentcons USING "DD/MM/YYYY",
     COLUMN 78,w_aux.paterno CLIPPED," ",w_aux.materno CLIPPED
              ," ",w_aux.nombres [1,9],
     COLUMN 108,w_aux.n_seguro USING "&&&&&&&&&&&",
     COLUMN 122,w_aux.n_unico,  
     COLUMN 142,w_aux.usuario              

   AFTER GROUP OF w_aux.nro_solic

     SKIP 2 LINES

     PRINT
     COLUMN 01,"SOLICITUDES APROBADAS POR SOLICITUD ",w_aux.nro_solic," : "
              ,GROUP COUNT(*) USING "&&&&&&"

     SKIP 2 LINES

   AFTER GROUP OF w_aux.lote

     SKIP 2 LINES

     PRINT
     COLUMN 01,"SOLICITUDES APROBADAS EN LOTE ",
                w_aux.lote USING "###"," : ",GROUP COUNT(*) USING "&&&&&&"

   ON LAST ROW

     PRINT 
     COLUMN 01,"TOTAL DE SOLICITUDES    :  ",COUNT(*) USING "&&&&&&&"

     SKIP 2 LINES

END REPORT


REPORT listado3(w_aux)
#l3-------------------
   DEFINE w_aux      RECORD
          lote       LIKE afi_solicitud.lote,
          agenc_cod  LIKE afi_solicitud.agenc_cod,
          nro_solic  LIKE pro_mae_promotor.nro_solicitud ,
          ppaterno   LIKE pro_mae_promotor.paterno,
          mmaterno   LIKE pro_mae_promotor.materno,
          nnombres   LIKE pro_mae_promotor.nombres,
          n_folio    LIKE afi_solicitud.n_folio,
          fentcons   LIKE afi_solicitud.fentcons,
          paterno    LIKE afi_solicitud.paterno ,
          materno    LIKE afi_solicitud.materno ,
          nombres    LIKE afi_solicitud.nombres ,
          n_seguro   LIKE afi_solicitud.n_seguro,
          n_unico    LIKE afi_solicitud.n_unico,
          usuario    LIKE afi_solicitud.usuario
   END RECORD

   DEFINE razon_social    CHAR(40)

   OUTPUT
   PAGE LENGTH 90
   LEFT MARGIN 0
   RIGHT MARGIN 0
   TOP MARGIN 0
   BOTTOM MARGIN 0

   FORMAT
   PAGE HEADER

     SELECT A.razon_social
     INTO   razon_social
     FROM   tab_afore_local A

     PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

     PRINT
     COLUMN 001,razon_social,
     COLUMN 140,"FECHA   : ",hoy USING "DD/MM/YYYY ",
     COLUMN 149,hora [1,5]

     IF g_reg.opcion = "1" THEN
        PRINT
        COLUMN 001,"AFIL012", 
        COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R    R E G I S T R O    A P R O B A D A S",   
        COLUMN 140,"P\240gina : ",pageno USING "#####"
     ELSE
        PRINT
        COLUMN 001,"AFIL012", 
        COLUMN 035,"S O L I C I T U D E S    D E    A F I L I A C I O N    P O R    T R A S P A S O    A P R O B A D A S",   
        COLUMN 140,"P\240gina : ",pageno USING "#####"
     END IF
      
     PRINT
     PRINT
     COLUMN 67,"Del : ",g_reg.fecha_ini USING "DD/MM/YYYY",
               " Al : ",g_reg.fecha_top USING "DD/MM/YYYY" 

     PRINT
     COLUMN 001,"----------------------------------------",
     COLUMN 040,"----------------------------------------",
     COLUMN 080,"----------------------------------------",
     COLUMN 120,"----------------------------------------",
     COLUMN 160,"---------"

     PRINT
     COLUMN 01,"Lote",
     COLUMN 06,"Estr",
     COLUMN 17,"N. Promo.",
     COLUMN 28,"Nombre Promotor",
     COLUMN 52,"Folio",
     COLUMN 64,"F. captura", 
     COLUMN 78, "Nombre",
     COLUMN 108,"N.S.S.",
     COLUMN 122,"C.U.R.P",
     COLUMN 142,"USUARIO"            

     PRINT
     COLUMN 001,"========================================",
     COLUMN 040,"========================================",
     COLUMN 080,"========================================",
     COLUMN 120,"========================================",
     COLUMN 160,"========="

   ON EVERY ROW
     LET estrc = NULL

     SELECT nombre_uni_n1 
     INTO   estrc
     FROM   com_nivel1
     WHERE  coduni_n1 = w_aux.agenc_cod

     PRINT
     COLUMN 01,w_aux.lote USING "##" ,
     COLUMN 07,estrc [1,6] ,
     COLUMN 17,w_aux.nro_solic,
     COLUMN 28,w_aux.ppaterno CLIPPED," ",w_aux.nnombres [1,13],
     COLUMN 52,w_aux.n_folio  USING "&&&&&&&&&",
     COLUMN 66,w_aux.fentcons USING "DD/MM/YYYY",
     COLUMN 78,w_aux.paterno CLIPPED," ",w_aux.materno CLIPPED
              ," ",w_aux.nombres [1,9],
     COLUMN 108,w_aux.n_seguro USING "&&&&&&&&&&&&",
     COLUMN 122,w_aux.n_unico,
     COLUMN 140,w_aux.usuario              

   AFTER GROUP OF w_aux.fentcons

     SKIP 2 LINES

     PRINT
     COLUMN 01,"SOLICITUDES APROBADAS EN FECHA ",w_aux.fentcons
                USING "DD/MM/YYYY"," : ",GROUP COUNT(*) USING "&&&&&&"

     SKIP 2 LINES

   AFTER GROUP OF w_aux.lote

     SKIP 2 LINES

     PRINT
     COLUMN 01,"SOLICITUDES APROBADAS EN LOTE ",
                w_aux.lote USING "###"," : ",GROUP COUNT(*) USING "&&&&&&"

     SKIP 2 LINES

   ON LAST ROW

     PRINT 
     COLUMN 01,"TOTAL DE SOLICITUDES    :  ",COUNT(*) USING "&&&&&&&"

      SKIP 2 LINES

END REPORT
