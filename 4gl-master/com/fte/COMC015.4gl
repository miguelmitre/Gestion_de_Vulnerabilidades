################################################################################
#Proyecto          => Sistema de Afores. ( MEXICO                              #
#Owner             => E.F.P.                                                   #
#Programa          => COMC015                                                  #
#Descripcion       => GENERACION DEL REPORTE DE PAGO DE COMISIONES             #
#Fecha             => 19 Ene  2005.                                            #
#By                => JOSE ALEJANDRO RAMIREZ.                                  #
#Sistema           => COM.                                                     #
------------------------------------------------------------------------------ #
#Datos a considerar=> comision = comis_pagada                                  #
#                  => adelanto = monto_comision                                #
#                  => celda    = (cod_esq_comision + num_sm) Porcentaje para   #
#                  =>                                        c/cliente         #
------------------------------------------------------------------------------ #
#Modify            => 23 Feb 2005 Jose alejandro Ramirez                       #
#Descr.            => (v4) Se nos solicito no sumar al num_sm y salario de ctas#
#                  => con num_sm < 3 (Gerentes y Gran total), ademas se agrego #
#                  => a los promotores la descr del esqu de comision.          #
#                  => Se asignaron cifras fijas para los topados.              #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE

      enter           char(01),
      g_param_dis     RECORD LIKE seg_modulo.* ,
      w_codigo_afore  LIKE tab_afore_local.codigo_afore,
      g_usuario       CHAR (08),


      g_reg0 RECORD
        fecha_corte  DATE,
        codven            CHAR(10) --v6
      END RECORD,

      g_reg ARRAY[1000] OF RECORD
        codven              LIKE com_comis_detalle.codven,       --v6
        coduni_n1           LIKE com_comis_detalle.coduni_n1,
        vnom                CHAR(55)
      END RECORD,

      g_reg3 ARRAY[1000] OF RECORD
        n_folio            LIKE com_comis_detalle.n_folio,
        nss                LIKE com_comis_detalle.nss,
        tipo_solicitud     LIKE com_comis_detalle.tipo_solicitud,
        fentcons           LIKE com_comis_detalle.fentcons,
        salario_base_comis LIKE com_comis_detalle.salario_base_comis,
        num_sm             LIKE com_comis_detalle.num_sm,
        monto_comision     LIKE com_comis_detalle.monto_comision,
        estado_comision    LIKE com_comis_detalle.estado_comision
      END RECORD,

   g_reg4 ARRAY[6] OF RECORD
       seguro         LIKE pro_mae_promotor.seguro,
       paterno        LIKE pro_mae_promotor.paterno,
       materno        LIKE pro_mae_promotor.materno,
       nombres        LIKE pro_mae_promotor.nombres
   END RECORD,

   g_reg5 ARRAY[6] OF RECORD
       seguro         LIKE pro_mae_promotor.seguro,
       nombre         CHAR(50)
   END RECORD,

   nombre CHAR(50),

   vmenu     CHAR(01),
   aux_pausa    CHAR(1),
   HOY  DATE,
   SW_1      SMALLINT,
   cla_sel      CHAR(1500),
   cla_sel2     CHAR(800),
   cla_sell     CHAR(800),
   vaccion   smallint,
   cla_where CHAR(800),
   vcomando  SMALLINT,
   opc  CHAR(01),
   total     DECIMAL(12,2),
   pagada    DECIMAL(12,2),
   registros INTEGER,
   longitud  integer,

   record_1  ARRAY[700] OF RECORD
      n_folio               LIKE afi_solicitud.n_folio,
      n_seguro           LIKE afi_solicitud.n_seguro,
      tipo_solicitud     LIKE afi_solicitud.tipo_solicitud,
      frecafor              LIKE afi_solicitud.fentcons,
      vfecha_corte       LIKE com_comis_detalle.fecha_corte,
      salario_base_comis LIKE afi_solicitud.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      monto_comision     LIKE com_comis_detalle.monto_comision
   END RECORD,

   vregis             INTEGER,
   cont_de_registros  INTEGER,
   sumtot_afil        INTEGER,
   sumtot_sm          DECIMAL(8,2),
   sumtot_promsm      DECIMAL(6,2),
   sumtot_comi        DECIMAL(12,2),
   vnom_jefe          CHAR(50)

   DEFINE fcodven           CHAR(10)
   DEFINE xcodven           CHAR(10)
   DEFINE xfecha_corte      CHAR(8)
   DEFINE xfecha_corte2     DATE

   DEFINE g_grep_00         CHAR(1000)
   DEFINE g_grep_03         CHAR(1000)
   DEFINE columnas          CHAR(200)
   DEFINE comando           CHAR(200)
   DEFINE ejecuta           CHAR(200)
   DEFINE tott_num_sm        LIKE com_comis_detalle.num_sm
   DEFINE ban_solo_admon    SMALLINT

   DEFINE arr_nivsuper ARRAY[10] OF RECORD
     cve_respon    CHAR(10),
     cencos        CHAR(10),
     cod_puesto    INTEGER,
     esq_comision  INTEGER,
     nom_respon    CHAR(60),
     int_ger       LIKE com_comis_detalle.codven,      --v7
     nom_unidad    CHAR(40),
     cencos_sup    CHAR(10)
   END RECORD

   DEFINE totmsal_pro  LIKE com_comis_detalle.salario_base_comis  --v3
   DEFINE sams_ger           DECIMAL(12,2)
   DEFINE sal_geren          DECIMAL(12,2)   --v4
   DEFINE vsm_geren          DECIMAL(12,2)   --v4
   DEFINE totvsm_pro   LIKE com_comis_detalle.num_sm   --v4

END GLOBALS

MAIN
        OPTIONS PROMPT LINE LAST,
        INPUT WRAP,
        ACCEPT KEY control-o

        DEFER INTERRUPT

        CALL STARTLOG("COMC015.log")

        SELECT  ruta_rescate
        INTO    g_param_dis.ruta_rescate
        FROM    seg_modulo
        WHERE   modulo_cod='com'

        SELECT  codigo_afore,USER
        INTO    w_codigo_afore,g_usuario
        FROM    tab_afore_local

        LET HOY = TODAY
        LET vregis = 0

        OPEN WINDOW ventana_1 AT 2,2 WITH FORM "COMC0151" ATTRIBUTE( BORDER)
        DISPLAY " COMC015   REPORTE DE PAGO DE COMISIONES ADELANTO 80% DETALLE                         " AT 3,1 ATTRIBUTE(REVERSE)

        DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)

        DISPLAY "                                                                               " AT 7,1 ATTRIBUTE(REVERSE)

        DISPLAY "                                                                                " AT 09,1 ATTRIBUTE(REVERSE)

        DISPLAY "                                                                                " AT 19,1 ATTRIBUTE(REVERSE)

        MENU "CONSULTA"
           COMMAND "Consulta" "Consulta Comisiones"
              CALL Inicializa2()
              CALL Consulta()
           COMMAND "Salir" "Salir del Programa"
              EXIT MENU
        END MENU
        CLOSE WINDOW ventana_1
END MAIN

FUNCTION Inicializa()
    define i smallint
        LET sw_1 = 0
        INITIALIZE g_reg TO NULL



        for i=1 to 9
           DISPLAY g_reg[i].* TO scr_1[i].*
        end for

        LET total = 0
        LET registros = 0
        DISPLAY registros TO scr_3.*

        CLEAR SCREEN
END FUNCTION

FUNCTION Inicializa2()
   LET cla_where=NULL
   LET cla_sel=NULL
   INITIALIZE g_reg0.* TO NULL    --g_reg2.* ojo
   DISPLAY BY NAME g_reg0.*        --,g_reg2.* ojo
END FUNCTION

FUNCTION Consulta()
   DEFINE
      pat CHAR(40),
      mat char(40),
      nom char(40),
      pos SMALLINT,
      xcodven LIKE com_comis_detalle.codven, --v5
      vvalor    CHAR(01)

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " [ESC] Procesar " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " [Ctrl-C] Salir " AT 1,63 ATTRIBUTE(REVERSE)


      LET INT_FLAG = FALSE
      CONSTRUCT cla_where
         ON    a.fecha_corte,
               a.codven      --v6
         FROM  com_comis_detalle.fecha_corte,
               com_comis_detalle.codven   --v6

         ON KEY (ESC)

         LET xfecha_corte  = get_fldbuf(com_comis_detalle.fecha_corte)
      -- LET xcodven       = get_fldbuf(com_comis_detalle.codven) --v5
         LET xcodven       = get_fldbuf(com_comis_detalle.codven) --v6

            LET vcomando = 2
            EXIT CONSTRUCT
         ON KEY (INTERRUPT)
            LET vcomando=1
            EXIT CONSTRUCT
       END CONSTRUCT



       IF vcomando = 1 THEN
          LET INT_FLAG = FALSE
          ERROR "Operacion abortada"
          LET vcomando = 0
          RETURN
       END IF

       IF xfecha_corte IS NULL OR xfecha_corte= ' ' THEN
          ERROR "Es preciso ingresar la fecha de corte"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF


       LET xfecha_corte2 = xfecha_corte[3,4]||'/'||xfecha_corte[1,2]||'/'||
                           xfecha_corte[5,8]

       IF xcodven IS NULL THEN
          ERROR "Es preciso ingresar codigo del promotor"
          SLEEP 3
          LET INT_FLAG = FALSE
          ERROR ""
          LET vcomando = 0
          RETURN
       END IF

       LET ban_solo_admon = 0

       IF xcodven='1234567' THEN
          LET cla_where = cla_where[1,26]
          LET ban_solo_admon = 1
       END IF

         LET cla_sel ="SELECT a.nivel,",
                           "a.agenc_cod,'",xfecha_corte2,
                           "', a.codven ",  --v5
                    "FROM   pro_mae_promotor a ",
                    "WHERE a.codven='",xcodven CLIPPED, --v5
                    "' ORDER BY 2"


         PREPARE claexe2 FROM cla_sel
         DECLARE cursor_2 SCROLL CURSOR FOR claexe2
         OPEN cursor_2

         CALL primer_row()
   DISPLAY " [Ctrl-I]Imprime  [Ctrl-P]Clientes promotor" AT 7,01 ATTRIBUTE(REVERSE)
         CALL ver_arreglo(xcodven)

END FUNCTION
FUNCTION primer_row()

   FETCH FIRST cursor_2 INTO g_reg0.fecha_corte  --,g_reg2.* ojo
   IF STATUS=100 THEN
      ERROR "No hay registros en esta direccion "
   ELSE
      LET g_reg0.fecha_corte=xfecha_corte2
      LET g_reg0.codven=xcodven   --v6

      DISPLAY BY NAME g_reg0.*
   END IF

END FUNCTION



FUNCTION imprime(ban_agreg,ycodven)

   DEFINE G_IMPRE    CHAR(2000)
   DEFINE G_LISTA    CHAR(300)
   DEFINE impresion  CHAR(300)
   DEFINE tcodven    LIKE com_comis_detalle.codven  --v6
   DEFINE ycodven    LIKE com_comis_detalle.codven
   DEFINE tcoduni_n1 LIKE com_comis_detalle.coduni_n1
   DEFINE ban_agreg  SMALLINT

   DEFINE nuevo RECORD
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      codven             LIKE com_comis_detalle.codven,  --v5
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      fentcons           LIKE com_comis_detalle.fentcons,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      monto_comision     LIKE com_comis_detalle.monto_comision,
      comis_pagada       LIKE com_comis_detalle.comis_pagada,
      cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision, --v2
      nombre_pro         CHAR(60),
      nombre_cli         CHAR(60),
      tipo_solicitud     SMALLINT,
      fentcons_afi       DATE,
      niv_sup            CHAR(10)
   END RECORD,

   mandar RECORD
      n_folio            LIKE com_comis_detalle.n_folio,
      nss                LIKE com_comis_detalle.nss,
      codven             LIKE com_comis_detalle.codven,  --v6
      coduni_n1          LIKE com_comis_detalle.coduni_n1,
      fentcons           LIKE com_comis_detalle.fentcons,
      salario_base_comis LIKE com_comis_detalle.salario_base_comis,
      num_sm             LIKE com_comis_detalle.num_sm,
      monto_comision     LIKE com_comis_detalle.monto_comision,
      comis_pagada       LIKE com_comis_detalle.comis_pagada,
      cod_esq_comision   LIKE com_comis_detalle.cod_esq_comision, --v2
      nombre_pro         CHAR(45),
      nombre_cli         CHAR(45),
      tipo_solicitud     SMALLINT,
      niv_sup            CHAR(10)
   END RECORD,

      xpaterno           CHAR(40),
      xmaterno           CHAR(40),
      xnombres           CHAR(40),
      xpaterno2          CHAR(40),
      xmaterno2          CHAR(40),
      xnombres2          CHAR(40),
      vdescrip           CHAR(40),
      vcausa             CHAR(100)

   DEFINE val_porcenx    DECIMAL(12,2)

    IF ban_agreg = 1 THEN

       LET cla_sel ="SELECT a.n_folio,               ",
                   "a.nss,                          ",
                   "a.codven,                       ", --v6
                   "a.coduni_n1,                    ",
                   "a.fentcons,                     ",
                   "a.salario_base_comis,           ",
                   "a.num_sm,                       ",
                   "a.monto_comision,               ",
                   "a.comis_pagada,                 ",
                   "a.cod_esq_comision,             ",
                   "a.tipo_solicitud,               ",
                   "a.fentcons,                     ",
                   "b.uni_superior_n1               ",
                   "FROM   com_comis_detalle a,     ", --quitar
                   "outer  com_nivel1 b             ",
                   "WHERE ",cla_where CLIPPED        ,
                   " AND   a.nivel = 1              ",
                   " AND   a.coduni_n1=b.coduni_n1 ",  --v4
                 " AND a.monto_comision <> 0 ",        --v7
                   "  ORDER BY 3,1 "  CLIPPED           --v4


    ELSE
      LET cla_sel ="SELECT a.n_folio,               ",
                   "a.nss,                          ",
                   "a.codven,                       ", --v6
                   "a.coduni_n1,                    ",
                   "a.fentcons,                     ",
                   "a.salario_base_comis,           ",
                   "a.num_sm,                       ",
                   "a.monto_comision,               ",
                   "a.comis_pagada,                 ",
                   "a.cod_esq_comision,             ",
                   "a.tipo_solicitud,               ",
                   "a.fentcons,                     ",
                   "b.uni_superior_n1               ",
                   "FROM   com_comis_detalle a,     ", --quitar
                   "outer  com_nivel1 b             ",
                   "WHERE ",cla_where CLIPPED        ,
                   " AND   a.nivel = 1              ",
                   " AND   a.coduni_n1=b.coduni_n1 ",  --v4
                 " AND a.monto_comision <> 0 ",        --v7
                   " ORDER BY 3,1 " CLIPPED            --v4



    END IF




      WHENEVER ERROR CONTINUE
      DROP TABLE temp_listo

      LET totmsal_pro = 0   --v3
      LET tott_num_sm = 0
      LET sams_ger    = 0
      LET sal_geren = 0  --v4
      LET vsm_geren = 0  --v4
      LET totvsm_pro = 0 --v4

      CREATE TEMP TABLE temp_listo
      (
        codven char(10),
        nom_pro char(50),
        cencos char(10),
        folio_clie integer,
        nss_clie char(11),
        nom_clie char(50),
        fec_certif date,
        sala_base_comis decimal(12,2),
        num_sm decimal(6,2),
        monto_comis decimal(12,2),
        comis_pagada decimal(12,2),
        edo_comis smallint,
        descr_edo CHAR(40),
        descr_causa CHAR(70)
      )

      WHENEVER ERROR STOP

      LET G_IMPRE = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                                ".R_COMC015_",HOY USING "DDMMYYYY"

      LET G_LISTA = g_param_dis.ruta_rescate CLIPPED,"/",g_usuario CLIPPED,
                                ".A_COMC015_",HOY USING "DDMMYYYY"

      -- PARA IMPRESION
      START REPORT rpt_cuenta_imp TO G_IMPRE

    ERROR "Preparando la informacion para imprimir..."
         PREPARE claexe8 FROM cla_sel

         DECLARE cursor_12 CURSOR FOR claexe8
         FOREACH cursor_12 INTO  nuevo.n_folio,
                        nuevo.nss,
                        nuevo.codven,  --v6
                        nuevo.coduni_n1,
                        nuevo.fentcons,
                        nuevo.salario_base_comis,
                        nuevo.num_sm,
                        nuevo.monto_comision,
                        nuevo.comis_pagada,
                        nuevo.cod_esq_comision,
                        nuevo.tipo_solicitud,
                        nuevo.fentcons_afi,
                        nuevo.niv_sup


       INITIALIZE mandar.* TO NULL

         LET mandar.n_folio             = nuevo.n_folio
         LET mandar.nss                 = nuevo.nss
         LET mandar.codven         = nuevo.codven  --v6
         LET mandar.coduni_n1           = nuevo.coduni_n1
         LET mandar.fentcons            = nuevo.fentcons
         LET mandar.salario_base_comis  = nuevo.salario_base_comis
         LET mandar.num_sm              = nuevo.num_sm
         LET mandar.monto_comision      = nuevo.monto_comision
         LET mandar.comis_pagada        = nuevo.comis_pagada
         LET mandar.cod_esq_comision    = nuevo.cod_esq_comision
         LET mandar.niv_sup             = nuevo.niv_sup


         LET  xpaterno2 = ''
         LET  xmaterno2 = ''
         LET  xnombres2 = ''

         SELECT paterno, materno, nombres
         INTO   xpaterno2,xmaterno2,xnombres2
         FROM   afi_mae_afiliado a
         WHERE  a.n_seguro  = nuevo.nss

         IF     xpaterno2 IS NULL THEN
                LET xpaterno2 = ' '
         END IF

         IF     xmaterno2 IS NULL THEN
                LET xmaterno2 = ' '
         END IF

         IF     xnombres2 IS NULL THEN
                LET xnombres2 = ' '
         END IF

         LET mandar.nombre_cli = xpaterno2 CLIPPED||' '||
                                 xmaterno2 CLIPPED||' '||
                                 xnombres2 CLIPPED

         LET xpaterno=''
         LET xmaterno=''
         LET xnombres=''

         SELECT paterno,materno,nombres
         INTO   xpaterno,xmaterno,xnombres
         FROM   pro_mae_promotor
         WHERE  codven = nuevo.codven --v6

         IF     xpaterno IS NULL THEN
                LET xpaterno = ' '
         END IF

         IF     xmaterno IS NULL THEN
                LET xmaterno = ' '
         END IF

         IF     xnombres IS NULL THEN
                LET xnombres = ' '
         END IF

         LET mandar.nombre_pro = xpaterno CLIPPED||' '||
                                 xmaterno CLIPPED||' '||
                                 xnombres CLIPPED

         IF     xpaterno2 IS NULL THEN
                LET xpaterno2 = ' '
         END IF

         IF     xmaterno2 IS NULL THEN
                LET xmaterno2 = ' '
         END IF

         IF     xnombres2 IS NULL THEN
                LET xnombres2 = ' '
         END IF

         LET mandar.nombre_cli = xpaterno2 CLIPPED||' '||
                                 xmaterno2 CLIPPED||' '||
                                 xnombres2 CLIPPED

         LET mandar.tipo_solicitud = nuevo.tipo_solicitud

         --Obtengo el porcentaje por cliente
         CALL Porcentaje(mandar.cod_esq_comision,mandar.num_sm)
                         RETURNING val_porcenx

         --Llamo a la funcion que trae el niverl superior
         CALL  Obtine_niv_super(mandar.coduni_n1)
               RETURNING arr_nivsuper[1].cencos_sup,
                         arr_nivsuper[1].nom_respon,
                         arr_nivsuper[1].int_ger,       --v7
                         arr_nivsuper[1].nom_unidad,
                         arr_nivsuper[1].esq_comision,
                         arr_nivsuper[2].cencos_sup,
                         arr_nivsuper[2].nom_respon,
                         arr_nivsuper[2].nom_unidad

   LET vregis = vregis + 1

   -- PARA LA IMPRESION

   OUTPUT TO REPORT rpt_cuenta_imp(mandar.*,val_porcenx,vcausa,
                                   arr_nivsuper[1].cencos_sup,
                                   arr_nivsuper[1].nom_respon,
                                   arr_nivsuper[1].int_ger,        --v7
                                   arr_nivsuper[1].nom_unidad,
                                   arr_nivsuper[1].esq_comision,
                                   arr_nivsuper[2].cencos_sup,
                                   arr_nivsuper[2].nom_respon,
                                   arr_nivsuper[2].nom_unidad)

   END FOREACH

   FINISH REPORT rpt_cuenta_imp
   ERROR "LISTADO 1 GENERADO...."
   SLEEP 2
   ERROR ""

{
   unload to G_LISTA
   select * from temp_listo order by 1,11 desc

   LET columnas = "COD PROMOTOR'|'NOM PROMOTOR'|'CENTRO COSTOS'|'FOLIO CLIEN'|'NSS CLIE'|',",
                  "NOM CLIEN'|'FEC CERTIF'|'SALAR BASE COMI'|'SAMS'|'MONTO COMIS'|'ESTADO COMIS'|'DESCRIP EDO'|'DESCRIP CAUSA"

   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; echo ",columnas CLIPPED,"> alguno2" CLIPPED
   RUN comando


   LET comando ="cd ",g_param_dis.ruta_rescate CLIPPED,"; cat ",G_LISTA CLIPPED,">> alguno2" CLIPPED,";mv alguno2 ",G_LISTA CLIPPED
   RUN comando
}

 LET impresion = "lp ",G_IMPRE
  RUN impresion

END FUNCTION


REPORT rpt_cuenta_imp(lnuevo,val_porcen,vcausa_edo,
                                         niv_super,
                                         nom_ger,
                                         int_ger,   --v7
                                         unidad_ger,
                                         esq_comi_ger,
                                         niv_super2,
                                         nom_sub,
                                         unidad_sub)


  DEFINE
    lnuevo RECORD
      n_folio                    LIKE com_comis_detalle.n_folio,
      nss                        LIKE com_comis_detalle.nss,
     codven                      LIKE com_comis_detalle.codven, --v6
      coduni_n1                  LIKE com_comis_detalle.coduni_n1,
      fentcons                   LIKE com_comis_detalle.fentcons,
      salario_base_comis         LIKE com_comis_detalle.salario_base_comis,
      num_sm                     LIKE com_comis_detalle.num_sm,
      monto_comision             LIKE com_comis_detalle.monto_comision,
      comis_pagada               LIKE com_comis_detalle.comis_pagada,
      cod_esq_comision           LIKE com_comis_detalle.cod_esq_comision, --v2
      nombre_pro                 CHAR(45),
      nombre_cli                 CHAR(45),
      tipo_solicitud             SMALLINT,
      niv_sup                    CHAR(10)   --nuevo
   END RECORD

   DEFINE niv_super              CHAR(10)
   DEFINE nom_ger                CHAR(50)
   DEFINE int_ger                LIKE com_comis_detalle.codven   --v7
   DEFINE unidad_ger             CHAR(50)
   DEFINE puesto_ger             SMALLINT
   DEFINE esq_comi_ger           SMALLINT

   DEFINE niv_super2             CHAR(10)
   DEFINE nom_sub                CHAR(50)
   DEFINE unidad_sub             CHAR(50)
   DEFINE puesto_sub             SMALLINT

   DEFINE vdescrip_edo           CHAR(40)
   DEFINE vcausa_edo             CHAR(100)

   DEFINE val_porcen             DECIMAL(12,2)
   DEFINE val_porcen_ger         DECIMAL(12,2)

      --enter                      char(01)

   DEFINE subt_num_sm        DECIMAL(12,2)
   DEFINE desc_Esquema           CHAR(50)
   DEFINE desc_esquema_promo     CHAR(50)   --v3

   DEFINE msal_pro           LIKE com_comis_detalle.salario_base_comis --v3
   DEFINE mvsm_pro           LIKE com_comis_detalle.salario_base_comis --v4
 OUTPUT
      TOP MARGIN 0
      BOTTOM MARGIN 0
      LEFT MARGIN   0
      RIGHT MARGIN  0
      PAGE LENGTH   90
      ORDER BY niv_super,lnuevo.niv_sup,
               lnuevo.coduni_n1,lnuevo.codven,lnuevo.n_folio, --v4 --v6
               lnuevo.cod_esq_comision desc   --v2
  FORMAT
     PAGE HEADER

      PRINT '\033e\033(10U\033&l1O\033&k2S\033&l12d'
      PRINT COLUMN 02,'\033(s7B',"COMC015                                                                                                                                     ",today USING "dd-mm-yyyy",'\033(s0B'
      SKIP 2 LINE
      PRINT COLUMN 80,'\033(s7B',"REPORTE DE PAGO DE COMISIONES",'\033(s0B'
      PRINT COLUMN 80,'\033(s7B',"  ADELANTO DEL 80% DETALLE",'\033(s0B'
      SKIP 1 LINE


    BEFORE GROUP OF niv_super
       PRINT COLUMN 002,'NOMBRE DEL SUBDIRECTOR'
       PRINT COLUMN 002,nom_sub
       PRINT COLUMN 002,'Subdirector'
       PRINT COLUMN 002,'Centro de costos:',niv_super
       PRINT COLUMN 002,'Nombre de la unidad:',unidad_sub
       SKIP 2 LINE

        --  AFTER GROUP OF niv_super         totales por subdireccion

    BEFORE GROUP OF lnuevo.niv_sup

      LET sal_geren = 0  --v4
      LET vsm_geren = 0  --v4

      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '______________________________________'
       SKIP 1 LINE
       PRINT COLUMN 002,'NOMBRE DEL GERENTE '
       PRINT COLUMN 002,nom_ger
       PRINT COLUMN 002,'Gerencia: ',unidad_ger
     --PRINT COLUMN 002,'Centro de costos:',lnuevo.niv_sup
       PRINT COLUMN 002,'No. INT.: ',int_ger

      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '_______________________'
       SKIP 2 LINE

      --  AFTER GROUP OF lnuevo.niv_sup    totales por gerencia


      BEFORE GROUP OF lnuevo.codven        --v6


      LET subt_num_sm = 0
      LET msal_pro = 0     --v3
      LET mvsm_pro = 0     --v4

      SKIP 1 LINE

      --Obtengo la descrip del esquema del promotor    --v3
      SELECT desc_esq_comision                         --v3
      INTO   desc_esquema_promo                        --v3
      FROM   com_esq_comis                             --v3
      WHERE  cod_esq_comision = lnuevo.cod_esq_comision--v3

      IF desc_esquema_promo IS NULL THEN               --v3
         LET desc_esquema_promo = 'ESQUEMA CERO'       --v3
      END IF                                           --v3

      PRINT COLUMN 01,'\033(s7B',"Num promotor     Nombre promotor                     Grp Venta    Fecha corte        Esquema   ",'\033(s0B'
      PRINT COLUMN  001,'__________________________________________________',
                        '__________________________________________________',
                        '__________________________________________________',
                        '_______________________'
      SKIP 1 LINE
      PRINT COLUMN 002,lnuevo.codven CLIPPED,   --v6
            COLUMN 013,lnuevo.nombre_pro CLIPPED,
            COLUMN 057,lnuevo.coduni_n1 CLIPPED,
            COLUMN 067,xfecha_corte2 USING "DD-MM-YYYY",
            COLUMN 085,desc_esquema_promo

      SKIP 1 LINE



      PRINT COLUMN 05,"FOLIO        NSS        NOMBRE DEL TRABAJADOR                    TIPO SOLICITUD         SALARIO       SAMS   %CELDA           COMISION         ANTICIPO"

      SKIP 1 LINE

    LET sams_ger = 0

    ON EVERY ROW

      --Para los topados
      IF lnuevo.num_sm > 25 THEN                                   --v4
         LET lnuevo.salario_base_comis = 1170                      --v4
         LET lnuevo.num_sm             =   25                      --v4
      END IF                                                       --v4

      --Para la masa salarial del promotor                         --v3
      IF lnuevo.num_sm > 3 THEN                                    --v3
         LET msal_pro    = msal_pro    + lnuevo.salario_base_comis --v3
         LET totmsal_pro = totmsal_pro + lnuevo.salario_base_comis --v3
         LET mvsm_pro    = mvsm_pro    + lnuevo.num_sm             --v3
         LET totvsm_pro  = totvsm_pro  + lnuevo.num_sm             --v3

      END IF                                                       --v3

      PRINT COLUMN 004,lnuevo.n_folio              USING "&&&&&&&&&",
            COLUMN 015,lnuevo.nss          CLIPPED,
            COLUMN 028,lnuevo.nombre_cli,
            COLUMN 073,lnuevo.tipo_solicitud       USING "##",
            COLUMN 085,lnuevo.salario_base_comis   USING "####,###,#&&.&&",
            COLUMN 102,lnuevo.num_sm               USING "##,##&.&&",
            COLUMN 112,val_porcen                  USING "###&.&&",'%', --v2
       COLUMN 123,lnuevo.comis_pagada              USING "####,###,#&&.&&",
       column 140,lnuevo.monto_comision            USING "####,###,#&&.&&"

      AFTER GROUP OF lnuevo.niv_sup

      LET sams_ger = sams_ger + GROUP SUM(lnuevo.num_sm)

      --Obtengo el porcen del gerente
      CALL Porcentaje(esq_comi_ger,sams_ger) RETURNING val_porcen_ger



      --Obtengo la descrip del esquema del gerente
      SELECT desc_esq_comision
      INTO   desc_esquema
      FROM   com_esq_comis
      WHERE  cod_esq_comision = esq_comi_ger

      IF desc_esquema IS NULL THEN
         LET desc_esquema = 'ESQUEMA CERO'
      END IF

      SKIP  1 LINES
      PRINT COLUMN 085,'____________________________TOTALES POR GERENCIA_____________________________________'

      PRINT COLUMN 085,sal_geren USING "#####,###,#&&.&&","   ",vsm_geren USING "####&.&&","           ",GROUP SUM(lnuevo.comis_pagada) * val_porcen_ger USING "####,###,#&&.&&","  ",GROUP SUM(lnuevo.monto_comision) USING "####,###,#&&.&&"," ",GROUP SUM(lnuevo.comis_pagada) USING "####,###,#&&.&&"
      PRINT COLUMN 085,'       SUBT_SALA.  SUBT_SAMS                SUBT_COMIS.       SUBT_ANTICI.  COMISI_EJECU.'
      SKIP  2 LINES

      ----@@@@
      AFTER GROUP OF lnuevo.codven            --v6
      LET sal_geren = sal_geren + msal_pro    --v4
      LET vsm_geren = vsm_geren + mvsm_pro    --v4

      PRINT COLUMN 085,'_____________________________________________________________________________________'


      PRINT COLUMN 085,msal_pro USING "#####,###,#&&.&&","   ",mvsm_pro USING "####&.&&","           ",GROUP SUM(lnuevo.comis_pagada) USING "####,###,#&&.&&"," ",GROUP SUM(lnuevo.monto_comision) USING "####,###,#&&.&&"," ",GROUP SUM(lnuevo.comis_pagada) USING "####,###,#&&.&&"
      PRINT COLUMN 085,'       TOTA_SALA.  TOTA_SAMS                TOTA_COMIS.     TOTA_ANTICI.  COMISI_EJECU.'
      SKIP  2 LINES

      LET tott_num_sm = tott_num_sm + subt_num_sm


      SKIP TO TOP OF PAGE
      ---------------SKIP  2 LINES   OJO


      PAGE TRAILER
      SKIP 2 LINE
      PRINT COLUMN 60," Pagina : ",PAGENO USING "<<<<<"


      ON LAST ROW

      PRINT COLUMN 085,'________________________________________GRAN TOTAL :_________________________________'
      PRINT COLUMN 085,'_____________________________________________________________________________________'

      PRINT COLUMN 085,totmsal_pro USING "########,#&&.&&","   ",totvsm_pro USING "####&.&&","           ",SUM(lnuevo.comis_pagada) * val_porcen_ger USING "####,###,#&&.&&","  ",SUM(lnuevo.monto_comision) USING "####,###,#&&.&&"," ",SUM(lnuevo.comis_pagada) USING "####,###,#&&.&&"
      PRINT COLUMN 085,'       TOTA_SALA.  TOTA_SAMS                TOTA_COMIS.       TOTA_ANTICI.  COMISI_EJECU.'
      PRINT COLUMN 085,'_____________________________________________________________________________________'
      PRINT COLUMN 085,'_____________________________________________________________________________________'

      SKIP  3 LINES

END REPORT

#-----------------------------------------------------------------------------
# Obtiene el query principal
#-----------------------------------------------------------------------------

FUNCTION ver_arreglo(xcodven2)
   DEFINE
      pat    CHAR(40),
      mat    char(40),
      nom    char(40),
      pos    SMALLINT,
      fhasta DATE,
      i      SMALLINT,
      xcodven2 CHAR(10),
      ban_agregados SMALLINT

       LET total = 0
       LET pagada = 0
       LET registros = 0
       LET ban_agregados = 0

       LET cla_sel2="SELECT a.codven,a.coduni_n1,",    --v6
                    "trim(b.nombres)||' '||trim(b.paterno)||' '||",
                    "trim(b.materno)",
        "FROM   com_comis_detalle a,pro_mae_promotor b ", --quitar
                    "WHERE ",cla_where CLIPPED,
                    " AND    a.codven=b.codven ",  --v5
                    " GROUP BY 1,2,3 " CLIPPED,
                    " ORDER BY 2,1 " CLIPPED

       ERROR "Buscando Informacion"

       PREPARE claexe FROM cla_sel2
       DECLARE cursor_1111 CURSOR FOR claexe
       LET pos = 1
       FOREACH cursor_1111 INTO g_reg[pos].*


                 LET registros = registros  + 1
                 LET pos = pos + 1
                 IF pos >= 9000 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH

         DISPLAY registros TO scr_3.*

         ERROR ""

         CALL  SET_COUNT(pos-1)

         IF (pos-1) >= 1 THEN
            DISPLAY ARRAY g_reg TO scr_1.*

               ON KEY (CONTROL-I)
                  LET i = ARR_CURR()
                  CALL imprime(ban_agregados,g_reg[i].codven)  --v6

               ON KEY (CONTROL-P)
                  LET i = ARR_CURR()
                  CALL Busca_afiliados(g_reg[i].codven,g_reg[i].coduni_n1)  --v6

               ON KEY (INTERRUPT)
                  EXIT DISPLAY
            END DISPLAY
         ELSE

           LET ban_agregados = 1
           --********************************************************
           --Para cuando se trate de un promotor sin afiliados y sin
           --aportaciones pero si pendientes y rechazados
           --********************************************************
           LET cla_sell="SELECT a.codven,a.agenc_cod,",  --v5
                    "trim(a.nombres)||' '||trim(a.paterno)||' '||",
                    "trim(a.materno) ",
                    "FROM   pro_mae_promotor a ",
                    "WHERE  a.codven='",xcodven2,   --v5
                    "' GROUP BY 1,2,3 " CLIPPED,
                    " ORDER BY 1,2 " CLIPPED


           ERROR "Buscando Informacion"

           PREPARE claexel FROM cla_sell
           DECLARE cursor_1111l CURSOR FOR claexel
           LET pos = 1
           FOREACH cursor_1111l INTO g_reg[pos].*



                     LET registros = registros  + 1
                     LET pos = pos + 1
                     IF pos >= 9000 THEN
                        ERROR "Sobrepaso la capacidad del arreglo"
                        EXIT FOREACH
                     END IF
           END FOREACH

           DISPLAY registros TO scr_3.*

           ERROR ""

           CALL  SET_COUNT(pos-1)

           IF (pos-1) >= 1 THEN
              DISPLAY ARRAY g_reg TO scr_1.*

                 ON KEY (CONTROL-I)
                  LET i = ARR_CURR()
                    CALL imprime(ban_agregados,g_reg[i].codven)  --v6

                 ON KEY (CONTROL-P)
                    LET i = ARR_CURR()
                    CALL Busca_afiliados(g_reg[i].codven,g_reg[i].coduni_n1)  --v6

                 ON KEY (INTERRUPT)
                    EXIT DISPLAY
              END DISPLAY
          ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""


          END IF
      END IF  --del else


      call Inicializa()
      CLEAR SCREEN
END FUNCTION

#-----------------------------------------------------------------------------
# Obtiene los afiliados
#-----------------------------------------------------------------------------

FUNCTION Busca_afiliados(vcodven,vcoduni_n1)

   DEFINE registros2 INTEGER
   DEFINE reg1       INTEGER
   DEFINE reg2       INTEGER,
          monto1     LIKE com_comis_detalle.monto_comision,
          monto2     LIKE com_comis_detalle.monto_comision,
          mto_comi   LIKE com_comis_detalle.monto_comision,
          pos        SMALLINT,
          parte1     CHAR(26),
          parte2     CHAR(10),
          parte3     CHAR(100),
          i          SMALLINT,
          vcodven    CHAR(10),
          vcoduni_n1 LIKE com_comis_detalle.coduni_n1,
          vcod_tipo  LIKE com_comis_detalle.cod_tipo_prom,
          vfec_corte LIKE com_comis_detalle.fecha_corte,
          vestado    LIKE com_comis_detalle.estado_comision
   DEFINE calidad1   LIKE com_comis_detalle.num_sm
   DEFINE calidad2   LIKE com_comis_detalle.num_sm



   OPEN WINDOW ventana_2 AT 9,2 WITH FORM "COMC0152"

   DISPLAY "                         CLIENTES DEL PROMOTOR                                " AT 2,1 ATTRIBUTE(REVERSE)


   DISPLAY "             [Ctrl-C] Salir                          [Enter] Nombre Cliente    " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 10,1 ATTRIBUTE(REVERSE)


   LET registros2 = 0
   LET reg1 = 0
   LET reg2 = 0

   LET monto1      = 0
   LET monto2      = 0
   LET mto_comi    = 0

   LET cla_sel2="SELECT n_folio,",
                "nss,",
                          "tipo_solicitud,",
                "fentcons,",
                "salario_base_comis, ",
                          "num_sm, ",
                          "monto_comision, ",
                          "estado_comision ",
                "FROM   com_comis_detalle ",  --quitar
                "WHERE codven = '",vcodven CLIPPED,
                "' AND coduni_n1 = '",vcoduni_n1 CLIPPED,
                "' AND fecha_corte = '",xfecha_corte2 CLIPPED,
                "' AND monto_comision <> 0 ",  --v7
                " ORDER BY estado_comision" CLIPPED

         ERROR "Buscando Informacion"

         PREPARE claexe_pend FROM cla_sel2
         DECLARE cursor_pend2 CURSOR FOR claexe_pend

         LET pos = 1
         FOREACH cursor_pend2 INTO g_reg3[pos].*
                 LET registros2 = registros2 + 1

       --Para el monto de la comision
       LET mto_comi = mto_comi + g_reg3[pos].monto_comision

       LET pos = pos + 1
       IF pos >= 9000 THEN
             ERROR "Sobrepaso la capacidad del arreglo"
             EXIT FOREACH
       END IF
    END FOREACH

    DISPLAY registros2 TO scr_1.registros2
    DISPLAY mto_comi TO scr_1.mto_comi


         ERROR ""

         CALL  SET_COUNT(pos-1)


         IF (pos-1) >= 1 THEN
            DISPLAY ARRAY g_reg3 TO scr_0.*
               ON KEY (CONTROL-M)
                  LET i = ARR_CURR()
                  CALL Mostrar_nombre2(g_reg3[i].nss)
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
         END IF
         CLOSE WINDOW ventana_2
END FUNCTION




#-----------------------------------------------------------------------------
# Muestra el nombre de los afiliados en una ventana
#-----------------------------------------------------------------------------

FUNCTION Mostrar_nombre2(vnss)
   DEFINE vnss    CHAR(11),
          pos     INTEGER

   OPEN WINDOW ventana001 AT 09,2 WITH FORM "COMC0154"

   DISPLAY "                             NOMBRE DEL CLIENTE                                " AT 2,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                              [Ctrl-C] Salir   " AT 04,1 ATTRIBUTE(REVERSE)

   DISPLAY "                                                                               " AT 11,1 ATTRIBUTE(REVERSE)


   LET cla_sel2="SELECT a.n_seguro,",
                       "a.paterno,",
                       "a.materno,",
                       "a.nombres ",
                "FROM   afi_mae_afiliado a ",
                "WHERE  a.n_seguro  = '",vnss CLIPPED,
                "'"

         ERROR "Buscando Informacion"

         PREPARE claexe_cte2 FROM cla_sel2
         DECLARE cursor_cte2 CURSOR FOR claexe_cte2
         LET pos = 1
         FOREACH cursor_cte2 INTO g_reg4[pos].*
            LET g_reg5[pos].seguro = g_reg4[pos].seguro
            LET g_reg5[pos].nombre = g_reg4[pos].paterno CLIPPED," ",
                                     g_reg4[pos].materno CLIPPED," ",
                                     g_reg4[pos].nombres CLIPPED
                 LET pos = pos + 1
                 IF pos >= 900 THEN
                    ERROR "Sobrepaso la capacidad del arreglo"
                    EXIT FOREACH
                 END IF
         END FOREACH

         ERROR ""

         CALL  SET_COUNT(pos-1)

         IF (pos-1) >= 1 THEN
            DISPLAY ARRAY g_reg5 TO scr_0.*
               ON KEY (INTERRUPT)
                  EXIT DISPLAY
            END DISPLAY
         ELSE
            ERROR "NO EXISTEN DATOS CON ESTAS CONDICIONES"
            SLEEP 3
            error ""
         END IF
         CLOSE WINDOW ventana001
END FUNCTION


#-----------------------------------------------------------------------------
# Obtiene nivel superior
#-----------------------------------------------------------------------------

FUNCTION Obtine_niv_super(cencos)

 ----GENERA LOS NIVELES SUPERIORES
 DEFINE cencos CHAR(10)
 DEFINE valor ARRAY[10] OF RECORD
     cve_respon    CHAR(10),
     cencos        CHAR(10),
     cod_puesto    INTEGER,
     esq_comision  INTEGER,
     nom_respon    CHAR(60),
      int_ger       LIKE com_comis_detalle.codven,   --v7
     nom_unidad    CHAR(40),
     cencos_sup    CHAR(10)
 END RECORD

 DEFINE opc CHAR(01)

 --JEFE NIVEL 1
 SELECT dat.cod_resp_uni,
        niv.coduni_n1,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.nombre_resp_uni,
        res.codven,            --v7
        niv.nombre_uni_n1,
        niv.uni_superior_n1
 INTO
        valor[1].cve_respon,
        valor[1].cencos,
        valor[1].cod_puesto,
        valor[1].esq_comision,
        valor[1].nom_respon,
        valor[1].int_ger,   --v7
        valor[1].nom_unidad,
        valor[1].cencos_sup
 FROM   com_nivel1        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n1    = cencos
 AND    dat.cod_uni      = niv.coduni_n1
 AND    dat.nivel        = 1
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto


 SELECT dat.cod_resp_uni,
        niv.coduni_n2,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.nombre_resp_uni,
        niv.nombre_uni_n2,
        niv.uni_superior_n2
 INTO
        valor[2].cve_respon,
        valor[2].cencos,
        valor[2].cod_puesto,
        valor[2].esq_comision,
        valor[2].nom_respon,
        valor[2].nom_unidad,
        valor[2].cencos_sup
 FROM   com_nivel2        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n2    = valor[1].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n2
 AND    dat.nivel        = 2
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto

 SELECT dat.cod_resp_uni,
        niv.coduni_n3,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.nombre_resp_uni,
        niv.nombre_uni_n3,
        niv.uni_superior_n3
 INTO
        valor[3].cve_respon,
        valor[3].cencos,
        valor[3].cod_puesto,
        valor[3].esq_comision,
        valor[3].nom_respon,
        valor[3].nom_unidad,
        valor[3].cencos_sup
 FROM   com_nivel3        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n3    = valor[2].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n3
 AND    dat.nivel        = 3
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto

 SELECT dat.cod_resp_uni,
        niv.coduni_n4,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.nombre_resp_uni,
        niv.nombre_uni_n4,
        niv.uni_superior_n4
 INTO
        valor[4].cve_respon,
        valor[4].cencos,
        valor[4].cod_puesto,
        valor[4].esq_comision,
        valor[4].nom_respon,
        valor[4].nom_unidad,
        valor[4].cencos_sup
 FROM   com_nivel4        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n4    = valor[3].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n4
 AND    dat.nivel        = 4
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto


 SELECT dat.cod_resp_uni,
        niv.coduni_n5,
        tab.cod_puesto,
        tab.cod_esq_comision,
        res.nombre_resp_uni,
        niv.nombre_uni_n5,
        niv.uni_superior_n5
 INTO
        valor[5].cve_respon,
        valor[5].cencos,
        valor[5].cod_puesto,
        valor[5].esq_comision,
        valor[5].nom_respon,
        valor[5].nom_unidad,
        valor[5].cencos_sup
 FROM   com_nivel5        niv,
        com_dat_uni_com   dat,
        com_respon_unidad res,
        tab_puesto        tab
 WHERE  niv.coduni_n5    = valor[4].cencos_sup
 AND    dat.cod_uni      = niv.coduni_n5
 AND    dat.nivel        = 5
 AND    res.cod_resp_uni = dat.cod_resp_uni
 AND    res.puesto_resp  = tab.cod_puesto

 RETURN valor[1].cencos_sup,
        valor[1].nom_respon,
        valor[1].int_ger,   --v7
        valor[1].nom_unidad,
        valor[1].esq_comision,
        valor[2].cencos_sup,
        valor[2].nom_respon,
        valor[2].nom_unidad


END FUNCTION


#---------------------------------------------------------------
FUNCTION Porcentaje(vcod_esq_comision,vnum_sm)

  DEFINE vcod_esq_comision   LIKE com_cuadro_comis.cod_esq_comision
  DEFINE vnum_sm             LIKE com_cuadro_comis.monto_comision
  DEFINE vporcentaje         LIKE com_cuadro_comis.monto_comision

   LET vporcentaje = 0

   SELECT monto_comision
   INTO   vporcentaje
   FROM   com_cuadro_comis
   WHERE  cod_esq_comision = vcod_esq_comision
   AND    rango_desde     <= vnum_sm
   AND    rango_hasta     >= vnum_sm

-- LET vporcentaje  = vporcentaje / 100  --v3
   LET vporcentaje  = vporcentaje        --v3

   RETURN vporcentaje
END FUNCTION


