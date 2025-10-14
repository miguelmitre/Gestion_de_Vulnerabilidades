##############################################################################
#Owner             => E.F.P.
#Programa TRAM200  => BUSQUEDA SOLICITUDES ICEFA BDSAR92    
#Fecha creacion    => 04 DE MAYO DE 1999
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 15 marzo 2006           
#Sistema           => TRA-IMSS
##############################################################################

DATABASE safre_af 
GLOBALS

############################
 DEFINE g_nss_cap   CHAR(011)
 DEFINE reg_afi_mae_afiliado RECORD LIKE afi_mae_afiliado.*
 DEFINE i                                ,
        pos                              ,
        ind                              ,
        g_ind_busqueda                   ,
        valida_modulo                    ,
        folio_reg                        SMALLINT

 DEFINE c_busqueda                       CHAR(300),
        x_buscar                         CHAR(300),
        c8_usuario                       CHAR(008),
        afi_paterno                               ,
        afi_materno                               ,
        afi_nombre                       CHAR(040)

 DEFINE reg_1 RECORD #glo #reg_1
           n_folio               LIKE afi_mae_afiliado.n_folio        ,
           tipo_solicitud        LIKE afi_mae_afiliado.tipo_solicitud ,
           n_seguro              LIKE afi_mae_afiliado.n_seguro       ,
           n_rfc                 LIKE afi_mae_afiliado.n_rfc          ,
           n_unico               LIKE afi_mae_afiliado.n_unico        ,
           fentcons              LIKE afi_mae_afiliado.fentcons       ,
           paterno               LIKE afi_mae_afiliado.paterno        ,
           materno               LIKE afi_mae_afiliado.materno        ,
           nombres               LIKE afi_mae_afiliado.nombres
 END RECORD

DEFINE reg_2 RECORD 
       icefa_cod       char(003),
       rfc             char(013),
       nro_int_cta     char(030),
       nss             char(030)
END RECORD
       
DEFINE a_reg_3 ARRAY[1000] OF RECORD LIKE tra_det_bdsar92.*

DEFINE a_reg_4 ARRAY[1000] OF RECORD 
       rfc             char(013),
       nro_int_cta     char(030),
       nss             char(011),
       n_seguro        char(011),
       fecha_edo       date
END RECORD 
DEFINE reg_5 RECORD 
       rfc             char(013),
       icefa_cod       char(003),
       des_icefa       char(020),
       nro_int_cta     char(030),
       nss             char(011),
       pat             char(040),
       mat             char(040),
       nom             char(040)
END RECORD


############################

   DEFINE b_window  SMALLINT
   DEFINE ejecuta_p CHAR(100)
   DEFINE b_uaep   SMALLINT
   DEFINE folio_uaep INTEGER
   DEFINE txt      CHAR(1500)
   DEFINE ffb      SMALLINT
   DEFINE x_busca  CHAR(100)
   DEFINE usuario  CHAR(010)
   DEFINE v_mod SMALLINT
   DEFINE rr SMALLINT
   DEFINE vtipo SMALLINT
   DEFINE vestado  SMALLINT
   DEFINE ruta char(100)
   DEFINE reg_tra_det_aut_issste RECORD LIKE tra_det_aut_issste.*
   DEFINE v_corr integer
   DEFINE sw smallint
   DEFINE reg_param_tra RECORD LIKE seg_modulo.*
   DEFINE band  CHAR(01)

   DEFINE #glo #char
         enter                 CHAR(1) ,
         HORA                  CHAR(8)
   DEFINE v_folio_interno INTEGER
   DEFINE v_correlativo INTEGER
   DEFINE HOY             DATE

   DEFINE b SMALLINT

   DEFINE  g_glob            RECORD
           codigo_afore      LIKE safre_af:tab_afore_local.codigo_afore,
           razon_social      LIKE safre_af:tab_afore_local.razon_social
                             END RECORD

   DEFINE  g_nom_prog        CHAR(07)

END GLOBALS  

MAIN

    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST

     CALL init()

        CALL despliega_folios()

END MAIN

FUNCTION despliega_folios()
#-------------------------


DEFINE f_b SMALLINT
DEFINE arr_tra_ctr_folio ARRAY[100] OF RECORD LIKE tra_ctr_folio.*

DEFINE reg_tra_ctr_folio RECORD LIKE tra_ctr_folio.*

DEFINE arr_c                ,
       i             SMALLINT


LET f_b = 1

    OPEN WINDOW tram2001 AT 2,2 WITH FORM "TRAM2001" ATTRIBUTE(BORDER)
    DISPLAY" TRAM200        BUSQUEDA DE SOLICITUD EN BASE DE DATOS SAR 92                          " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                            CRITERIOS DE BUSQUEDA                                   " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY " Rfc Icefa     Numero de control Icefa         Nss Icefa   Nss Afore  Fecha    " AT 12,1 ATTRIBUTE(REVERSE)

       LET b_window = 0

	MENU "MENU DE ENVIO DE CARTAS INVITACION"
		COMMAND "Confirma" "Confirma envio de cartas invitacion"
           CALL dibuja_pantalla(13,19,15)
           CALL dibuja_pantalla(13,19,46)
           CALL dibuja_pantalla(13,19,58)
           CALL dibuja_pantalla(13,19,70)
        DISPLAY "                                                                               " AT 20,1 ATTRIBUTE(REVERSE)
         CALL despliega_confronta()
           
      COMMAND "Salir" "Salir pantalla anterior"
           EXIT MENU
	END MENU
{
   IF f_b = 0 THEN

     SELECT "OK" 
     FROM rep_aut
     GROUP BY 1

     IF STATUS <> NOTFOUND THEN

        START REPORT listado_1 TO ruta

        DECLARE cur_rep CURSOR FOR

         SELECT A.*
         FROM  tra_det_aut_issste A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
         AND    A.estado IN (160)
         ORDER BY A.estado

         FOREACH cur_rep INTO  reg_tra_det_aut_issste.*
            OUTPUT TO REPORT listado_1(reg_tra_det_aut_issste.*)
         END FOREACH 
         FINISH REPORT  listado_1
     END IF
     CLOSE WINDOW TRAM0711 
   --  EXIT PROGRAM
   END IF
}
END FUNCTION

FUNCTION despliega_confronta()
#dc---------------------------

DEFINE f_folio          INTEGER
DEFINE f_cve_desc       CHAR(50)
DEFINE f_criterio_cod   SMALLINT 
DEFINE f_criterio_desc  CHAR(030)
DEFINE f_correlativo    INTEGER
DEFINE f_icefa_cod      CHAR(05)  

DEFINE festado,
       arr_c                ,
       i             SMALLINT,
       tot_reg_pant  SMALLINT 
define v_pru   char(03)

########################################################

    LET folio_reg = FALSE

    INPUT BY NAME reg_1.n_folio        , 
                  reg_1.tipo_solicitud ,
                  reg_1.n_seguro       ,
                  reg_1.n_rfc          ,
                  reg_1.n_unico     WITHOUT DEFAULTS

        AFTER FIELD n_folio
           IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_unico
            END IF

            IF reg_1.n_folio IS NULL THEN
                NEXT FIELD n_seguro
            END IF


        AFTER FIELD tipo_solicitud
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio
            END IF

            IF  reg_1.tipo_solicitud > 5 THEN
                ERROR"TIPO SOLICITUD INVALIDO"
                NEXT FIELD tipo_solicitud
            END IF

            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons       ,
                   paterno        ,
                   materno        ,
                   nombres        ,
                   USER
            INTO   reg_1.*,c8_usuario
            FROM   afi_mae_afiliado
            WHERE  n_folio        = reg_1.n_folio
            AND    tipo_solicitud = reg_1.tipo_solicitud

            IF STATUS = NOTFOUND THEN
                ERROR "FOLIO NO EXISTE EN afi_mae_afiliado....NO ES UN AFILIADO ",
                      "DE LA AFORE"
                NEXT FIELD n_folio
            ELSE 
               LET folio_reg = TRUE
            END IF
    DISPLAY "<CTRL-B> Criterios Busqueda <CTRL-C>SALIR                                                  " AT 20,1 ATTRIBUTE(REVERSE)
            DISPLAY BY NAME reg_1.*
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF
        AFTER FIELD n_seguro
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_folio
            END IF

            IF reg_1.n_seguro IS NULL THEN
                NEXT FIELD n_rfc
            END IF

            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons       ,
                   paterno        ,
                   materno        ,
                   nombres        ,
                   USER
            INTO   reg_1.*,c8_usuario
            FROM   afi_mae_afiliado
            WHERE  n_seguro = reg_1.n_seguro

            IF STATUS = NOTFOUND THEN
                ERROR " NSS INEXISTENTE "
                NEXT FIELD n_seguro
            ELSE
                 LET folio_reg = TRUE
            END IF
    DISPLAY "<CTRL-B> Criterios Busqueda <CTRL-C>SALIR                                                  " AT 20,1 ATTRIBUTE(REVERSE)
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF
        AFTER FIELD n_rfc
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_seguro
            END IF

            IF reg_1.n_rfc IS NULL THEN
                NEXT FIELD n_unico
            END IF
            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons       ,
                   paterno        ,
                   materno        ,
                   nombres        ,
                   USER
            INTO   reg_1.*,c8_usuario
            FROM   afi_mae_afiliado
            WHERE  n_rfc = reg_1.n_rfc

            IF STATUS = NOTFOUND THEN
                ERROR " RFC INEXISTENTE "
                NEXT FIELD n_rfc
            ELSE
                  LET folio_reg = TRUE
            END IF 
    DISPLAY "<CTRL-B> Criterios Busqueda <CTRL-C>SALIR                                                  " AT 20,1 ATTRIBUTE(REVERSE)
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF

        AFTER FIELD n_unico
            IF FGL_LASTKEY() = FGL_KEYVAL("UP") THEN
                NEXT FIELD n_rfc
            END IF

            IF reg_1.n_unico IS NULL THEN
                NEXT FIELD n_folio
            END IF

            SELECT n_folio        ,
                   tipo_solicitud ,
                   n_seguro       ,
                   n_rfc          ,
                   n_unico        ,
                   fentcons       ,
                   paterno        ,
                   materno        ,
                   nombres        ,
                   USER
            INTO   reg_1.*,c8_usuario
            FROM   afi_mae_afiliado
            WHERE  n_unico = reg_1.n_unico

            IF STATUS = NOTFOUND THEN
                ERROR " CURP INEXISTENTE "
                NEXT FIELD n_unico
            ELSE
                LET folio_reg = TRUE
            END IF
    DISPLAY "<CTRL-B> Criterios Busqueda <CTRL-C>SALIR                                                  " AT 20,1 ATTRIBUTE(REVERSE)
            DISPLAY BY NAME reg_1.n_folio THRU reg_1.nombres
	    IF reg_1.tipo_solicitud = 5 THEN
           CALL despliega_mens(reg_1.n_seguro,"Nss asignado aun no vigente  :")
	   NEXT FIELD n_seguro
	   END IF

      ON KEY(CONTROL-B) 
   DISPLAY " <ESC> Buscar <Ctrl-C> Salir                                                   " AT 20,1 ATTRIBUTE(REVERSE)
         CALL busqueda()
      ON KEY (INTERRUPT)
         DISPLAY "                                                                               " AT 20,1 ATTRIBUTE(REVERSE)
         INITIALIZE reg_1.* TO NULL
         DISPLAY BY NAME reg_1.* 
         EXIT INPUT
    END INPUT

END FUNCTION

FUNCTION init() 
#i------------- 

LET HOY                               =                                 TODAY 
LET HORA                              =                                 TIME
LET g_nom_prog                        =                                "TRAM071"
LET g_ind_busqueda = 0

    CREATE TEMP TABLE rep_aut(correlativo integer)
     
    SELECT *
    INTO   reg_param_tra.*
    FROM  seg_modulo
    WHERE modulo_cod = "tra"

    SELECT user
    INTO   usuario
    FROM   tab_afore_local

    LET ruta = reg_param_tra.ruta_listados       CLIPPED,
    	       "/",usuario CLIPPED ,".TRAM071." ,
	       HOY USING "YYYYMMDD"    CLIPPED,".",
               HORA CLIPPED

    SELECT codigo_afore,
           razon_social
    INTO g_glob.*
    FROM safre_af:tab_afore_local


END FUNCTION

REPORT listado_1(reg_tra_det_aut_issste)
#l1--------------------------------------

DEFINE reg_tra_det_aut_issste RECORD LIKE tra_det_aut_issste.*
DEFINE i_cont_reg_total integer
DEFINE i_cont_reg_orig integer

    OUTPUT
        PAGE LENGTH 90

    FORMAT

    PAGE HEADER
IF sw = 0 
THEN LET i_cont_reg_total = 0
LET i_cont_reg_orig = 0
LET sw = 1
END IF
        PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
        PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,g_glob.codigo_afore USING "###","  ",g_glob.razon_social CLIPPED
        PRINT
            COLUMN 001,HOY USING"DD-MM-YYYY" ,
            COLUMN 155,g_nom_prog CLIPPED
        PRINT
            COLUMN 001,HORA,
            COLUMN 050,"REPORTE DE VALIDACION MANUAL CRUCE SAR 92 TRAS",
                       "PASO ICEFA-AFORE ISSSTE"
       PRINT
            COLUMN 001,"NSS"         ,
            COLUMN 013,"NSS ICEFA"   ,
            COLUMN 025,"RFC ICEFA"   ,
            COLUMN 040,"BANCO"       ,
            COLUMN 049,"NRO.INTERNO" 
            PRINT
            COLUMN 001,"NOMBRE"      
       PRINT
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT

    BEFORE GROUP OF reg_tra_det_aut_issste.estado
        LET i_cont_reg_orig = 0
        PRINT
        PRINT
       PRINT
            COLUMN 001,"----------------------------------------------------",
                       "----------------------------------------------------",
                       "----------------------------------------------------",
                       "-----------------" 
        PRINT
        PRINT
            COLUMN 001,"ULTIMO ESTADO :",
                       reg_tra_det_aut_issste.estado 
        PRINT
        PRINT
    ON EVERY ROW
        LET i_cont_reg_orig  = i_cont_reg_orig + 1
       -- LET i_cont_reg_total = i_cont_reg_total + 1
        PRINT
            COLUMN 001,reg_tra_det_aut_issste.n_seguro             ,
            COLUMN 013,reg_tra_det_aut_issste.n_seguro_ent         ,
            COLUMN 025,reg_tra_det_aut_issste.rfc_ent              ,
            COLUMN 040,reg_tra_det_aut_issste.cve_ced_cuenta       ,
            COLUMN 049,reg_tra_det_aut_issste.nro_ctrl_icefa       
            PRINT
            COLUMN 001,reg_tra_det_aut_issste.nombre_ent[1,40]     ,
            COLUMN 042,reg_tra_det_aut_issste.nombre_ent[41,80]    ,
            COLUMN 084,reg_tra_det_aut_issste.nombre_ent[81,120]
        PRINT
            
    AFTER GROUP OF reg_tra_det_aut_issste.estado
       -- LET i_cont_reg_total = i_cont_reg_total + i_cont_reg_orig
        PRINT
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS :",reg_tra_det_aut_issste.estado,
                       " :          ",i_cont_reg_orig USING"#########"
        PRINT 	

    ON LAST ROW

         SELECT COUNT(*)
         INTO i_cont_reg_total
         FROM  tra_det_aut_issste A
         WHERE  A.correlativo IN 
               (SELECT unique H.correlativo
                FROM   rep_aut H)
         AND    A.estado IN (160)

        PRINT	
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
        PRINT
            COLUMN 001,"TOTAL DE REGISTROS MODIFICADOS MANUALMENTE :       ",
                       i_cont_reg_total USING"#########"
        PRINT      
            COLUMN 001,"====================================================",
                       "====================================================",
                       "====================================================",
                       "=================" 
      
                                                       
END REPORT
FUNCTION trae_estado(vestado_cod)
#te-----------------------------


DEFINE vestado_cod SMALLINT
DEFINE v_estado_desc  CHAR(040)

     SELECT a.estado_descripcion
     INTO v_estado_desc
     FROM  tra_aut_estado a
     WHERE a.estado_cod = vestado_cod      


RETURN vestado_cod,v_estado_desc

END FUNCTION

{FUNCTION uno()
#u------------
DEFINE cad CHAR(300)

LET cad = "fglgo TRAM072"
RUN cad

END FUNCTION
}

FUNCTION consulta_estados()
#u------------
DEFINE cad CHAR(300)

LET cad = "fglgo TRAM073"
RUN cad

END FUNCTION


FUNCTION despliega_mens(vnss,cadena)
#di----------------------------------

DEFINE vnss CHAR(011)
DEFINE cadena CHAR(100)

  OPEN WINDOW tram00120 AT 12,20 WITH FORM "TRAM00120" ATTRIBUTE(BORDER)
    DISPLAY  cadena AT  4,3
    DISPLAY BY NAME vnss 
    SLEEP 4
    IF valida_modulo = 1 THEN
       EXIT PROGRAM
    END IF
  CLOSE WINDOW tram00120

END FUNCTION

FUNCTION busqueda()
#b-----------------

WHILE TRUE
       LET INT_FLAG = FALSE
       CONSTRUCT BY NAME x_buscar ON icefa_cod    ,
                                     rfc          ,
                                     nro_int_cta  ,
                                     nss
        ON KEY (ESC)
           EXIT CONSTRUCT
        ON KEY (INTERRUPT)
    DISPLAY "<CTRL-B> Criterios Busqueda <CTRL-C>SALIR                                                  " AT 20,1 ATTRIBUTE(REVERSE)
           LET g_ind_busqueda = 1
           EXIT CONSTRUCT 
       END CONSTRUCT 

     IF g_ind_busqueda = 1 THEN 
        LET g_ind_busqueda = 0
        EXIT WHILE
     ELSE 
        ERROR  "BUSCANDO INFORMACION EN LA B.D SAR92..." 
        CALL detalle() 
     END IF
END WHILE

END FUNCTION

FUNCTION detalle()
#det -------------

DEFINE brk SMALLINT
LET brk = 0

WHILE TRUE

    DISPLAY "<Enter> Detalle BDS92 <CTRL-W> Solicitar <CTRL-C>SALIR                                                    " AT 20,1 ATTRIBUTE(REVERSE)
       LET c_busqueda = " SELECT a.* " ,
                        " FROM   tra_det_bdsar92 a ",
                        " WHERE ",x_buscar ,
                        " ORDER BY a.rfc "

       PREPARE qry_busqueda FROM c_busqueda
       DECLARE cur_busqueda CURSOR FOR qry_busqueda
       LET pos = 1

     FOREACH cur_busqueda INTO a_reg_3[pos].*
        LET a_reg_4[pos].rfc          = a_reg_3[pos].rfc
        LET a_reg_4[pos].nro_int_cta  = a_reg_3[pos].nro_int_cta
        LET a_reg_4[pos].nss          = a_reg_3[pos].nss
        LET a_reg_4[pos].n_seguro     = a_reg_3[pos].n_seguro
        LET a_reg_4[pos].fecha_edo    = a_reg_3[pos].fecha_edo
        LET pos = pos + 1

        IF pos >= 1000 THEN 
           ERROR "CAPACIDAD MAXIMA DEL ARREGLO SOBREPASADA REFINE SU BUSQUEDA"
           EXIT FOREACH
        END IF
     END FOREACH

     ERROR ""

     IF pos = 1 THEN
      CALL despliega_mens("","Solicitud inexistente para criterio de busqueda")
      EXIT WHILE
     ELSE
 
      CALL SET_COUNT(pos-1)
      DISPLAY ARRAY a_reg_4 TO scr_4.*

      ON KEY(INTERRUPT)
         LET brk = 1
         LET pos = 0
         EXIT DISPLAY
      ON KEY(CONTROL-W)
         LET ind = ARR_CURR()

         SELECT "OK" 
         FROM   tra_mae_icefa a
         WHERE  a.nss         =  a_reg_3[ind].nss
         AND    a.rfc         =  a_reg_3[ind].rfc
         AND    a.icefa_cod   =  a_reg_3[ind].icefa_cod
         AND    a.nro_int_cta =  a_reg_3[ind].nro_int_cta
         GROUP BY 1

       IF STATUS = NOTFOUND THEN

         SELECT a.*    
         INTO   reg_afi_mae_afiliado.*
         FROM   safre_af:afi_mae_afiliado a
         WHERE  a.n_seguro = reg_1.n_seguro
      
         LET afi_paterno = a_reg_3[ind].nombre_ent[1,40] 
         LET afi_materno = a_reg_3[ind].nombre_ent[41,80] 
         LET afi_nombre  = a_reg_3[ind].nombre_ent[81,120] 
   
         LET v_corr = a_reg_3[ind].correlativo + 2000000
 
         ERROR "ACTUALIZANDO E INSERTANDO INFORMACION,POR FAVOR ESPERE..."
         INSERT INTO tra_mae_icefa
         VALUES(reg_afi_mae_afiliado.n_folio          ,
                reg_afi_mae_afiliado.tipo_solicitud   ,
                reg_afi_mae_afiliado.n_seguro         ,
                a_reg_3[ind].nss                      ,
                a_reg_3[ind].rfc                      ,
                afi_paterno                           ,
                afi_materno                           ,
                afi_nombre                            ,
                v_corr                                ,
                a_reg_3[ind].icefa_cod                ,
                a_reg_3[ind].nro_int_cta              ,
                TODAY                                 ,
                ""                                    ,
                ""                                    ,
                0                                     ,#sar_92_issste
                0                                     ,#viv_92_issste
                TODAY                                 ,
                TODAY                                 ,
                ""                                    ,#lote_genera
                ""                                    ,#fecha_genera
                20                                    ,#status
                4                                     ,#fuente
                0                                     ,#correlativo
                --a_reg_3[ind].correlativo             ,#correlativo
                c8_usuario                            ,
                0                                     ,#n_envios
                ""                                    )#diagnostico

                --2                                     )# cve_sector
        
             UPDATE tra_det_bdsar92
             SET    estado         = 20    ,
                    fecha_edo      = today ,
                    n_seguro       = reg_1.n_seguro
             WHERE  correlativo = a_reg_3[ind].correlativo
         ERROR ""

             LET a_reg_3[ind].n_seguro = reg_1.n_seguro
             CALL despliega_mens(reg_1.n_seguro,"REGISTRO LIGADO AL NSS :")

             PROMPT "REGISTRO INGRESADO AL MAESTRO DE ICEFAS,TECLEE < ENTER >..." FOR enter
          ELSE
             SELECT max(a.n_seguro) 
             INTO   g_nss_cap
             FROM   tra_mae_icefa a 
             WHERE  a.nss         =  a_reg_3[ind].nss
             AND    a.rfc         =  a_reg_3[ind].rfc
             AND    a.icefa_cod   =  a_reg_3[ind].icefa_cod
             AND    a.nro_int_cta =  a_reg_3[ind].nro_int_cta
             AND    a.fuente      <> 4

          IF STATUS <> NOTFOUND THEN
            IF (a_reg_3[ind].n_seguro IS NULL OR 
               a_reg_3[ind].n_seguro = ""    OR 
               a_reg_3[ind].n_seguro = " ")   THEN

             UPDATE tra_det_bdsar92
             SET    estado         = 20    ,
                    fecha_edo      = today ,
                    n_seguro       = g_nss_cap
             WHERE  correlativo = a_reg_3[ind].correlativo

           CALL despliega_mens(g_nss_cap,"YA REGISTRADO SE ASIGNA NSS A BDS92")

             ELSE

           CALL despliega_mens(reg_1.n_seguro,"YA REGISTRADO CON ANTERIORIDAD")

             END IF
            END IF
          END IF
      ---- EXIT DISPLAY
      ON KEY(RETURN)
        LET ind = ARR_CURR()

           OPEN WINDOW tram2002 AT 15,3 WITH FORM "TRAM2002" ATTRIBUTE(BORDER)
           DISPLAY"                            DETALLE DEL REGISTRO                                          " AT 1,1 ATTRIBUTE(REVERSE)

           LET reg_5.rfc = a_reg_3[ind].rfc
           LET reg_5.icefa_cod = a_reg_3[ind].icefa_cod

           SELECT a.icefa_desc
           INTO   reg_5.des_icefa
           FROM   tab_icefa a 
           WHERE  icefa_cod = a_reg_3[ind].icefa_cod

           LET reg_5.nro_int_cta = a_reg_3[ind].nro_int_cta
           LET reg_5.nss = a_reg_3[ind].nss
           LET reg_5.pat = a_reg_3[ind].nombre_ent[001,040]
           LET reg_5.mat = a_reg_3[ind].nombre_ent[041,080]
           LET reg_5.nom = a_reg_3[ind].nombre_ent[081,120]
           DISPLAY BY NAME reg_5.*


           PROMPT "Oprima cualquier tecla para continuar..." FOR char enter
           CLOSE WINDOW tram2002
      END DISPLAY
     END IF 
     FOR i = 1 TO 7
       CLEAR scr_4[i].*
     END FOR
     IF brk = 1 THEN 
        LET brk = 0
        EXIT WHILE
     END IF
   END WHILE

   DISPLAY " <ESC> Buscar <Ctrl-C> Salir                                                   " AT 20,1 ATTRIBUTE(REVERSE)
END FUNCTION

FUNCTION dibuja_pantalla(ini_vert,fin_vert,pos_hor)
#dp------------------------------------------------

DEFINE i SMALLINT
DEFINE ini_vert     ,
       fin_vert     ,
       pos_hor      SMALLINT

FOR i = ini_vert TO fin_vert
    DISPLAY "|" AT i,pos_hor
END FOR

END FUNCTION

