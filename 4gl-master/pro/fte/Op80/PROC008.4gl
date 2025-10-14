################################################################################
#Proyecto          => SISTEMA DE AFORES ( MEXICO )                             #
#Sistema           => PRO                                                      #
#Programa PROC008  => ENVIO.ACTUALIZACION DE DATOS DEL AGENTE PROMOTOR         #
#Fecha             => 12 DE JUNIO DEL 2000                                     #
#By                => FRANCO ESTEBAN ULLOA VIDELA                              #
#Fecha Modificacion=> 29 DE MARZO DEL 2004                                     #
#Modificado Por    => LAURA EUGENIA CORTES GUZMAN                              #
#Modificado Por    => Isabel Fonseca Frias                                     #
#Fecha             => 18-09-2009                                               #
#Observacion       => Se actualiza de acuerdo a layout del 27/07/09            #
#                  => (v10)                                                    #
################################################################################
DATABASE safre_af
GLOBALS
   DEFINE
      reg_2           RECORD LIKE pro_mae_promotor.* ,
      rd              RECORD LIKE tab_delegacion.*   ,
      rf              RECORD LIKE tab_ciudad.*       ,
      g_afore         RECORD LIKE tab_afore_local.*  ,
      parametro       RECORD LIKE seg_modulo.*       

   DEFINE reg_3 RECORD
      tip_registr     CHAR(003) ,
      calle_numer     CHAR(040) ,
      deleg           CHAR(030) ,
      ciudad          CHAR(030) ,
      estado          CHAR(002) ,
      cod_postal      CHAR(005) ,
      telefono        CHAR(100) ,
      res_examen      CHAR(002) ,
      num_age_pro     CHAR(010) ,
      fec_nacimie     CHAR(008)
   END RECORD

   DEFINE
      fecha           ,
      HOY             DATE      ,
      borra_lineas    CHAR(200) ,
      ch              CHAR(220) ,
      RUTA            CHAR(200) ,
      G_LISTA_1       CHAR(200) ,
      G_LISTA_2       CHAR(200) ,
      enter           CHAR(001) ,
      g_usuario       CHAR(008) ,
      HORA            CHAR(008) ,
      vestad_desc     CHAR(040) ,
      nro_registros   INTEGER

END GLOBALS


MAIN

   CALL init() #i

   DEFER INTERRUPT
   OPTIONS
      INPUT WRAP           ,
      PROMPT LINE LAST     ,
      ACCEPT KEY CONTROL-I

   CALL STARTLOG("PROC008.log")
   OPEN WINDOW proc0081 AT 4,4 WITH FORM "PROC0081" ATTRIBUTE(BORDER)
   DISPLAY "                           < Ctrl-C > Salir                                    " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " PROC008   GENERA ARCHIVO ACTUALIZACION DE DATOS DEL PROMOTOR                  " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   WHILE TRUE
      PROMPT " ESTA SEGURO S/N " FOR CHAR enter
      IF enter MATCHES "[SsNn]" THEN
         IF enter MATCHES "[Ss]" THEN
            LET nro_registros = 0

            --- Cambia condicion Oct 2010
            --- status para identificar ACTIVO(1,4) INACTIVO(otro)
            --- status_interno ahora para modificacion
            ###SELECT COUNT(*)
            ###INTO   nro_registros
            ###FROM   pro_mae_promotor A
            ###WHERE  A.status         = 4
            ###AND    A.status_interno = 0

            SELECT COUNT(*)
            INTO   nro_registros
            FROM   pro_mae_promotor A
            WHERE  A.status_interno = 6

            IF nro_registros = 0 THEN
               PROMPT " NO HAY REGISTROS MODIFICADOS PARA ENVIAR...",
                      "<ENTER> PARA SALIR " FOR CHAR enter
               EXIT PROGRAM
            END IF

            EXIT WHILE
         ELSE
            PROMPT " PROCESO CANCELADO...<ENTER> PARA SALIR " FOR CHAR enter
            EXIT PROGRAM
         END IF
      END IF
   END WHILE

   DISPLAY " PROCESANDO INFORMACION " AT 19,2 ATTRIBUTE(REVERSE)

   CALL primer_paso() #pp

   DISPLAY " NRO DE REGISTROS PROCESADOS : ",nro_registros AT 10,15
   PROMPT " PROCESO FINALIZADO ...<ENTER> PARA SALIR " FOR CHAR enter

   CLOSE WINDOW proc0081

END MAIN


FUNCTION init()
#i-------------

   LET HORA     = TIME
   LET HOY      = TODAY

   SELECT *         ,
          USER
   INTO   g_afore.* ,
          g_usuario
   FROM   tab_afore_local

   SELECT *
   INTO   parametro.*
   FROM   seg_modulo
   WHERE  modulo_cod = "pro"

   LET RUTA = parametro.ruta_envio CLIPPED

END FUNCTION


FUNCTION primer_paso()
#pp-------------------

   DEFINE #loc #smallint
      sw_1          SMALLINT
                    
   DEFINE           
      lc_sexo       CHAR(1),
      lc_ef_nac     CHAR(2),
      lc_tipo_mod   CHAR(1)

   DECLARE cur_1 CURSOR FOR
        ###SELECT A.*
        ###FROM   pro_mae_promotor A
        ###WHERE  A.status         = 4 --------EN PROCESO DE MODIFICACION
        ###AND    A.status_interno = 0 --------CAPTURADO
                                       --- Cambia condicion Oct 2010 
                                       --- status para identificar ACTIVO(1,4) INACTIVO(otro) 
                                       --- status_interno ahora para modificacion
      SELECT  A.*
      FROM  pro_mae_promotor A
      WHERE  A.status_interno   = 6

      LET G_LISTA_1 = RUTA CLIPPED,"/mod"
      LET G_LISTA_2 = RUTA CLIPPED,"/MOD"
      LET nro_registros = 0

      START REPORT listado_1 TO G_LISTA_1

      FOREACH cur_1 INTO reg_2.*
         LET sw_1          = 0

         LET reg_3.calle_numer = reg_2.calle  CLIPPED ," ",
                                 reg_2.numero CLIPPED ," ",
                                 reg_2.dpto

         SELECT *
         INTO   rd.*
         FROM   tab_delegacion
         WHERE  deleg_cod = reg_2.deleg

         IF STATUS = NOTFOUND THEN
            LET sw_1          = 1

            PROMPT reg_2.cod_promotor," NO EXISTE DELEGACION ",reg_2.deleg USING"&&&&"
            FOR CHAR enter
            CONTINUE FOREACH
         ELSE
            LET reg_3.deleg = rd.deleg_desc
         END IF

         SELECT *
         INTO  rf.*
         FROM  tab_ciudad
         WHERE  ciudad_cod = reg_2.ciudad

         IF STATUS = NOTFOUND THEN
            LET sw_1          = 1

            PROMPT reg_2.cod_promotor," NO EXISTE CIUDAD ",reg_2.ciudad USING"########"
            FOR CHAR enter
            CONTINUE FOREACH
         ELSE
            LET reg_3.ciudad = rf.ciudad_desc

            IF  reg_3.ciudad IS NULL OR
                reg_3.ciudad = " "  THEN

                SELECT estad_desc
                INTO   vestad_desc
                FROM   tab_estado
                WHERE  estad_cod = reg_2.estado

                LET reg_3.ciudad = "LOCALIDAD DE ",vestad_desc
            END IF
         END IF

         LET reg_3.fec_nacimie = YEAR(reg_2.fnaci)  USING "&&&&",
                                 MONTH(reg_2.fnaci) USING "&&"  ,
                                 DAY(reg_2.fnaci)   USING "&&"

         -- Obtener sexo y edo_naci, con CURP --- Oct 2010
         -- tipo_mod con status(1,4) Activo otro Inactivo 
         LET  lc_sexo     =  NULL
         LET  lc_ef_nac   =  NULL

         CALL curp(reg_2.unico) RETURNING  lc_sexo,
                                           lc_ef_nac

         IF reg_2.status   = 1
         OR reg_2.status   = 4 THEN 
            LET  lc_tipo_mod  = ' '     -- Activo    Oct 2010
         ELSE
            LET  lc_tipo_mod  = '1'     -- Inactivo  Oct 2010
         END IF

         OUTPUT TO REPORT listado_1 ("304"               ,
                                     reg_2.nombres       ,
                                     reg_2.paterno       ,
                                     reg_2.materno       ,
                                     reg_2.rfc           ,
                                     reg_2.seguro        ,
                                     reg_2.unico         ,
                                     reg_3.calle_numer   ,
                                     reg_2.colonia       ,
                                     reg_3.deleg         ,
                                     reg_3.ciudad        ,
                                     reg_2.estado        ,
                                     reg_2.codpos        ,
                                     reg_2.fono          ,
                                     reg_2.resuelva      ,
                                     reg_2.cod_promotor  ,
                                     reg_3.fec_nacimie   ,
                                     reg_2.fecha_baja    ,
                                     reg_2.escolar       ,
                                     lc_sexo             ,
                                     lc_ef_nac           ,
                                     lc_tipo_mod 
                                    ) #l1

         ---  No se actualiza status .. queda como identificador de activo inactivo
         ---  Oct 2010
         --- UPDATE pro_mae_promotor
         --- SET    pro_mae_promotor.status_interno = 1            ,
         ---        pro_mae_promotor.status         = 4            ,
         ---        pro_mae_promotor.fenvio         = HOY
         --- WHERE  pro_mae_promotor.status_interno = 6
         --- AND    pro_mae_promotor.cod_promotor   = reg_2.cod_promotor

         UPDATE  pro_mae_promotor
         SET     status_interno  =  1,
                 fenvio          =  HOY
         WHERE   status_interno  =  6
         AND     cod_promotor    =  reg_2.cod_promotor

         INSERT INTO pro_envio_mod
         VALUES(reg_2.cod_promotor , HOY, 1)

         IF sw_1 = 0 THEN
            LET nro_registros = nro_registros+ 1
         END IF
      END FOREACH

   FINISH REPORT listado_1

   LET ch = "chmod 777 ",G_LISTA_1 CLIPPED
   RUN ch

   LET borra_lineas = "sed -e '/^$/d' ",
                      G_LISTA_1 CLIPPED,
                      " > ",
                      G_LISTA_2 CLIPPED
   RUN borra_lineas

   INSERT INTO pro_ctr_envio VALUES (HOY,"MOD",2,"",nro_registros,"")

END FUNCTION


REPORT listado_1(r_304, lc_sexo, lc_ef_nac, lc_tipo_mod)
#l1-----------------------------------------------------

   DEFINE r_304 RECORD #loc #r_304
      tip_registr           CHAR(003) ,
      nombres               CHAR(040) ,
      paterno               CHAR(040) ,
      materno               CHAR(040) ,
      rfc                   CHAR(013) ,
      nss                   CHAR(011) ,
      curp                  CHAR(018) ,
      calle_numer           CHAR(040) ,
      colonia               CHAR(030) ,
      deleg_munic           CHAR(030) ,
      ciudad                CHAR(030) ,
      estado                CHAR(002) ,
      codpos                CHAR(005) ,
      telefono              CHAR(100) ,
      res_examen            CHAR(002) ,
      cod_promotor          CHAR(010) ,
      fec_nacimie           CHAR(008) ,
      fecha_baja            DATE      ,
      escolar               CHAR(01)
   END RECORD

   DEFINE    lc_sexo        CHAR(1)
   DEFINE    lc_ef_nac      CHAR(2)
   DEFINE    lc_tipo_mod    CHAR(1)

   DEFINE r_000  RECORD
      tip_registr           CHAR(3) ,
      num_registr           CHAR(5) ,
      tam_registr           CHAR(3) ,
      tip_archivo           CHAR(3) ,
      cve_afore             CHAR(3) ,
      fec_transm            CHAR(8) ,
      lot_afore             CHAR(9)
   END RECORD,

   clave_afore LIKE tab_afore_local.codigo_afore

   OUTPUT
      LEFT MARGIN   0
      RIGHT MARGIN  0
      TOP MARGIN    0
      BOTTOM MARGIN 0
      PAGE LENGTH   2

   FORMAT
      ON EVERY ROW
         LET r_304.fec_nacimie = YEAR(reg_2.fnaci)  USING "&&&&",
                                 MONTH(reg_2.fnaci) USING "&&" ,
                                 DAY(reg_2.fnaci)   USING "&&"

         PRINT
             COLUMN 001,r_304.tip_registr                      ,
             COLUMN 004,r_304.cod_promotor                     ,
             COLUMN 014,r_304.nombres                          ,
             COLUMN 054,r_304.paterno                          ,
             COLUMN 094,r_304.materno                          ,
             COLUMN 134,r_304.rfc                              ,
             COLUMN 147,r_304.nss                              ,
             COLUMN 158,r_304.curp                             ,
             COLUMN 176,r_304.fec_nacimie                      ,
             COLUMN 184,lc_sexo                                ,
             COLUMN 185,lc_ef_nac                              ,
             COLUMN 187,r_304.calle_numer                      ,
             COLUMN 227,r_304.colonia                          ,
             COLUMN 257,r_304.deleg_munic                      ,
             COLUMN 287,r_304.ciudad                           ,
             COLUMN 317,r_304.estado        USING "&&"         ,
             COLUMN 319,r_304.codpos        USING "&&&&&"      ,
             COLUMN 324,r_304.telefono                         ,
 --            COLUMN 293,r_304.res_examen    USING "&&"         ,  --(v10)
 --            COLUMN 293,3 SPACES                               ,  --(v10)
 --            COLUMN 296,r_304.fec_nacimie                      ,
             COLUMN 424,r_304.escolar                          ,
             COLUMN 425,lc_tipo_mod                            ,
             COLUMN 426,25 SPACES

END REPORT


--- Copia rutina de PROC001.4gl  ... resuelve la situacion de sexo y ent nac Oct 2010
FUNCTION curp(lc_curp)
#---------------------

   DEFINE    lc_curp                   CHAR(18) 
   DEFINE    v_sexo                    CHAR(1) 
   DEFINE    v_edo_naci                CHAR(2) 
   DEFINE    s_edo_naci                SMALLINT

   LET  v_sexo     = " "
   LET  v_edo_naci = "  "

   -- Obtiene la clave de sexo
   LET  v_sexo     = lc_curp[11,11]

   CASE v_sexo
      WHEN "M"
         LET v_sexo = 2
      WHEN "H"
         LET v_sexo = 1
   END CASE

   -- Obtiene la clave de la entidad de nacimiento
   LET  v_edo_naci = lc_curp[12,13]

   SELECT  cve_ent
   INTO  s_edo_naci
   FROM  pro_ent_naci
  WHERE  cod_ent = v_edo_naci

   LET v_edo_naci  = s_edo_naci USING '&&'

   RETURN  v_sexo,
           v_edo_naci

END FUNCTION


