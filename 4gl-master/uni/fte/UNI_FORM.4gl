###############################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                            #
#Propietario       => E.F.P.                                                  #
#Programa          => UNI_FORM  /  DATOS PARA FORMATO                         #
#Fecha             => 17 de noviembre de 2005                                 #
#Actualizado       => MIGUEL ANGEL HERNANDEZ MARTINEZ                         #
###############################################################################
DATABASE safre_af

GLOBALS
   DEFINE reg_1 RECORD
          nss_uni      CHAR(11),
          nss_cta1     CHAR(11),
          fliquida     DATE,
          fecha_inicio DATE,
          fecha_fin    DATE
   END RECORD

   DEFINE nss_concatena  CHAR(100),
          pos            SMALLINT,
          pos1           SMALLINT,
          opc            CHAR(1),
          xxx_nss_cta1   CHAR(11),
          HOY            DATE,
          cuantos        SMALLINT
END GLOBALS
#############################################
MAIN
   LET HOY = TODAY

   OPEN WINDOW v1 AT 2,2 WITH FORM "UNI_FORM1" ATTRIBUTE(BORDER)
   DISPLAY "       [Esc]  Iniciar                                    [Ctrl-C]  Salir       " AT 1,1 ATTRIBUTE(REVERSE)
   DISPLAY " UNI_FORM       RECUPERACION NSS UNIFICACION PARA FORMATO                      " AT 3,1 ATTRIBUTE(REVERSE)
   DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

   CALL inicio()
   ERROR " INICIA PROCESO "
   CALL unificador()
   CALL unificados()

   CLOSE WINDOW v1
END MAIN
#############################################
FUNCTION inicio()

   LET reg_1.fecha_inicio = "01/01/1997"
   LET reg_1.fecha_fin    = "06/30/2005"

   INPUT BY NAME reg_1.fecha_inicio,
                 reg_1.fecha_fin WITHOUT DEFAULTS

      AFTER FIELD fecha_inicio
         IF reg_1.fecha_inicio IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD fecha_inicio
         END IF

      AFTER FIELD fecha_fin
         IF reg_1.fecha_fin IS NULL THEN
            ERROR "Campo NO puede ser NULO"
            NEXT FIELD fecha_fin
         END IF

         SELECT COUNT(*)
         INTO   cuantos
         FROM   uni_formato_1
         WHERE  fecha_inicio = reg_1.fecha_inicio
         AND    fecha_fin    = reg_1.fecha_fin

         IF cuantos > 0 THEN
            ERROR "PERIODO YA RECUPERADO ..."
            NEXT FIELD fecha_inicio
         END IF

      ON KEY (ESC)
         SELECT COUNT(*)
         INTO   cuantos
         FROM   uni_formato_1
         WHERE  fecha_inicio = reg_1.fecha_inicio
         AND    fecha_fin    = reg_1.fecha_fin

         IF cuantos > 0 THEN
            ERROR "PERIODO YA RECUPERADO ..."
            NEXT FIELD fecha_inicio
         END IF

         ERROR " INICIA PROCESO "
         EXIT INPUT

      ON KEY (INTERRUPT)
         ERROR " PROCESO CANCELADO "
         EXIT PROGRAM
   END INPUT

END FUNCTION
#############################################
FUNCTION unificador()

   DECLARE cur_1 CURSOR FOR
   SELECT  a.nss_uni,
           a.fliquida
   FROM    uni_unificador a
   WHERE   a.estado IN (95,96,100)
   AND     a.fliquida BETWEEN reg_1.fecha_inicio AND reg_1.fecha_fin
   ORDER BY 2,1

   LET pos = 0

   FOREACH cur_1 INTO reg_1.nss_uni,
                      reg_1.fliquida

      DECLARE cur_2 CURSOR FOR
      SELECT  b.nss_cta1
      FROM    uni_unificado b
      WHERE   b.estado IN (95,96,100)
      AND     b.fliquida = reg_1.fliquida
      AND     b.nss_uni = reg_1.nss_uni

      LET pos1 = 1

      FOREACH cur_2 INTO reg_1.nss_cta1

         IF pos1 = 1 THEN
            LET nss_concatena = reg_1.nss_cta1 CLIPPED
         ELSE
            LET nss_concatena = nss_concatena CLIPPED,
                                "," CLIPPED,
                                reg_1.nss_cta1 CLIPPED
         END IF

         LET pos1 = pos1 + 1

      END FOREACH

      LET pos1 = pos1 - 1
      LET pos = pos + 1

      INSERT INTO uni_formato_1
      VALUES (reg_1.nss_uni,
              1,                  --- consecutivo
              nss_concatena,      --- nss cta1 concatenado
              pos1,               --- num_cta1_conca
              reg_1.fliquida,     --- fecha_liquidacion
              reg_1.fecha_inicio, --- fecha inicio periodo
              reg_1.fecha_fin,    --- fecha fin periodo
              TODAY,              --- factualiza
              USER                --- usuario
             )
               
   END FOREACH
   
END FUNCTION
#############################################
FUNCTION unificados()

   DECLARE cur_3 CURSOR FOR
   SELECT  a.nss_cta1,
           a.nss_uni,
           a.fliquida
   FROM    uni_unificado a
   WHERE   a.estado IN (95,96,100)
   AND     a.fliquida BETWEEN reg_1.fecha_inicio AND reg_1.fecha_fin
   ORDER BY 2,1

   LET pos = 1

   FOREACH cur_3 INTO xxx_nss_cta1,
                      reg_1.nss_uni,
                      reg_1.fliquida

      LET nss_concatena = reg_1.nss_uni CLIPPED

      DECLARE cur_4 CURSOR FOR
      SELECT  b.nss_cta1
      FROM    uni_unificado b
      WHERE   b.estado IN (95,96,100)
      AND     b.fliquida = reg_1.fliquida
      AND     b.nss_uni = reg_1.nss_uni
      AND     b.nss_cta1 <> xxx_nss_cta1

      FOREACH cur_4 INTO reg_1.nss_cta1

          LET pos = pos + 1

          LET nss_concatena = nss_concatena CLIPPED,
                              "," CLIPPED,
                              reg_1.nss_cta1 CLIPPED

       END FOREACH

       INSERT INTO uni_formato_1
       VALUES (xxx_nss_cta1,
               2,                  --- consecutivo
               nss_concatena,      --- nss cta1 concatenado
               pos,                --- num_cta1_conca
               reg_1.fliquida,     --- fecha_liquidacion
               reg_1.fecha_inicio, --- fecha inicio periodo
               reg_1.fecha_fin,    --- fecha fin periodo
               TODAY,              --- factualiza
               USER                --- usuario
              )

       LET pos = 1               
   END FOREACH
END FUNCTION
#############################################
