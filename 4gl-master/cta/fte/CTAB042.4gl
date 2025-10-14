################################################################################
#Proyecto     => safre_af                                                      #
#Propietario  => E.F.P.                                                        #
#Programa     => CTAB042                                                       #
#Descripcion  => IDENTIFICACION POR EDAD                                       #
#Por          => MIGUEL ANGEL HERNANDEZ MARTINEZ                               #
#Fecha Mod    => 10 de Junio 2010                                              #
#Sistema      => CTA - Identificación por Edad                                 #
#Parametros   => Fecha_proceso,  estado    en cta_transf_edad a considerar     #
################################################################################
DATABASE safre_af

GLOBALS
   DEFINE gc_mensaje1           ,
          gc_mensaje2           ,
          gc_mensaje3           CHAR(100),
          gc_mensaje4           CHAR(100),
          gc_usuario            CHAR(8)  ,
          gc_respuesta          CHAR(001)

   DEFINE g_cancela             SMALLINT

   DEFINE gd_fecha_corte,
          gd_fecha_valuacion    DATE

   DEFINE arr_c,
          arr_l,
          arr_t                 SMALLINT,
          GUSER                 CHAR(08),
          hoy                   DATE,
          pos                   SMALLINT,
          pos_subcta            SMALLINT

   DEFINE gi_siefore,
          gi_estatus,
          gi_siefore_ced,
          gi_siefore_rec        SMALLINT

   DEFINE gi_folio_liq INTEGER

   DEFINE gar_consulta_liq      ARRAY[400] OF RECORD
             folio              INTEGER,
             subcuenta          SMALLINT,
             siefore_ced        SMALLINT,
             pesos_ced          DECIMAL(22,6),
             acciones_ced       DECIMAL(22,6),
             siefore_rec        SMALLINT,
             pesos_rec          DECIMAL(22,6),
             acciones_rec       DECIMAL(22,6)
      END RECORD

   DEFINE gr_reporte            RECORD
  	  afore_desc            CHAR(14),
          precio1               DECIMAL(7,6),
          precio2               DECIMAL(7,6),
          precio3               DECIMAL(7,6),
          precio4               DECIMAL(7,6),
          precio5               DECIMAL(7,6),
          siefore1              CHAR(9),
          siefore2              CHAR(9),
          siefore3              CHAR(9),
          siefore4              CHAR(9),
          siefore5              CHAR(9)
      END RECORD

   DEFINE gar_regimen           ARRAY[20] OF RECORD
          codigo_siefore        SMALLINT,
          desc_siefore          CHAR(30),
          total                 INTEGER
      END RECORD

   DEFINE f_orden_sel           CHAR(200),
          f_act_ind_eda         CHAR(200),
          f_act_ctr_proc        CHAR(200),
          v_comando             CHAR(1000)

   DEFINE reg                   RECORD
          fecha_corte           DATE,
          fecha_edad            DATE
      END RECORD

   DEFINE hay_reg               INTEGER
   DEFINE lc_query              CHAR(2000)

  DEFINE   lc_Proceso                  CHAR(10)
  DEFINE   ls_Etapa                    SMALLINT 
  DEFINE   li_Etapas                   SMALLINT 
  DEFINE   lc_Comenta                  CHAR(50)
  DEFINE   ld_FechaInicio              DATE
  DEFINE   li_Consecutivo              INTEGER
  DEFINE   ls_Control                  SMALLINT
  DEFINE   ld_fecha_proceso            DATE 
  DEFINE   ls_estado                   SMALLINT 
  DEFINE   ls_intento                  SMALLINT 
END GLOBALS

################################################################################
MAIN
   OPTIONS  PROMPT LINE LAST,
   INPUT    WRAP,
   ACCEPT   KEY CONTROL-O
   DEFER    INTERRUPT

   CALL     inicio()
   CALL     cambio_regimen()

END MAIN

################################################################################
FUNCTION inicio()

   LET    ld_fecha_proceso     =  ARG_VAL(1) 
   LET    ls_estado            =  ARG_VAL(2)

   SELECT USER
   INTO   GUSER
   FROM   safre_af:seg_modulo
   WHERE  modulo_cod = "cta"

   LET hoy = TODAY
   LET     f_orden_sel     =  " EXECUTE FUNCTION fn_transfiere_ie (?,?,?,?,?,?,?) "
   PREPARE p_orden_sel      FROM     f_orden_sel

   LET     f_act_ind_eda   =  " EXECUTE FUNCTION fn_actualiza_ind_edad (?,?,?) "
   PREPARE p_act_ind_eda FROM f_act_ind_eda

   LET     lc_query        =  "    SELECT  nss,                          ",
                              "            ind_edad                      ",
                              "      FROM  cta_transf_edad               ",
                              "     WHERE  fecha_corte = ?               ",
                              "       AND  cod_rechazo = ?               "
   PREPARE p_VerEdad           FROM      lc_query
   DECLARE d_VerEdad         CURSOR FOR  p_VerEdad

   LET     lc_query        =  "    SELECT  a.nss,                        ",
                              "            a.siefore,                    ",
                              "            a.siefore_ant,                ",
                              "            a.grupo_regimen               ",
                              "      FROM  cta_transf_ed_gpo a           ",
                              "     WHERE  a.nss         = ?             ",
                              "       AND  a.fecha_corte = ?             ",
                              "       AND  a.estado      = 0             "
   PREPARE p_VerGpo            FROM      lc_query
   DECLARE d_VerGpo          CURSOR FOR  p_VerGpo

   LET     lc_query        =  "    UPDATE  cta_transf_ed_gpo             ",
                              "       SET  estado          =  ?,         ",
                              "            folio_transf    =  ?,         ",
                              "            intento         =  ?,         ",
                              "            factualiza      =  TODAY      ",
                              "     WHERE  nss             =  ?          ",
                              "       AND  grupo_regimen   =  ?          ",
                              "       AND  siefore_ant     =  ?          ",
                              "       AND  siefore         =  ?          ",
                              "       AND  fecha_corte     =  ?          "
   PREPARE p_ActGpo            FROM      lc_query

   LET     lc_query        =  "    UPDATE  cta_transf_edad               ",
                              "       SET  cod_rechazo     = ?           ",
                              "     WHERE  nss             = ?           ",
                              "       AND  fecha_corte     = ?           "
   PREPARE p_ActEdad           FROM      lc_query

   LET     lc_query        =  "    SELECT c.nss,                         ",
                              "           c.ind_edad,                    ",
                              "           c.criterio,                    ",
                              "           c.cod_rechazo                  ",
                              "    FROM   cta_transf_edad   c            ",
                              "    WHERE  c.fecha_corte   = ?            "
   PREPARE p_VerEdaGpo         FROM      lc_query
   DECLARE d_VerEdaGpo       CURSOR FOR  p_VerEdaGpo

   LET     f_act_ctr_proc     =  " EXECUTE PROCEDURE f_actualtime(?, ?) "
   PREPARE p_act_ctr_proc      FROM     f_act_ctr_proc


END FUNCTION

#####################################################################
FUNCTION cambio_regimen()

   DEFINE     vnss                     CHAR(11),
              sw_procesados            INTEGER ,
              vind_edad                SMALLINT

   DEFINE     r RECORD
              nss                      CHAR(11),
              siefore                  SMALLINT,
              siefore_ant              SMALLINT,
              grupo_regimen            SMALLINT
   END RECORD

   DEFINE     x_existe                 SMALLINT,
              x_ind_edad               SMALLINT,
              x_rechazo                SMALLINT,
              x_folio                  INTEGER,
              x_tipo_traspaso          SMALLINT,
              x_medio                  SMALLINT,
              x_tipo_proceso           SMALLINT,
              x_rch_marca              SMALLINT,
              x_eje_transf             SMALLINT,
              sw_aceptado              INTEGER,
              sw_rechazado             INTEGER,
              opc                      CHAR(1)
   DEFINE     ls_ResCam                SMALLINT

   LET x_tipo_traspaso   =   13
   LET x_medio           =   10
   LET x_tipo_proceso    =   2

   LET sw_procesados     =   0
   LET ls_intento        =   1 

   FOREACH    d_VerEdad          USING  ld_fecha_proceso,
                                        ls_estado
                                  INTO  vnss,
                                        vind_edad

      LET     sw_procesados          =   sw_procesados + 1
      LET sw_aceptado                =   0
      LET sw_rechazado               =   0

      FOREACH d_VerGpo           USING  vnss,
                                        ld_fecha_proceso 
                                  INTO  r.*

         EXECUTE p_orden_sel     USING  r.nss,             --- nss
                                        vind_edad,         --- ind_edad
                                        r.grupo_regimen,   --- grupo_regimen
                                        r.siefore,         --- siefore
                                        x_tipo_proceso,    --- tipo_proceso
                                        x_tipo_traspaso,   --- tipo_traspaso
                                        x_medio            --- medio
                                  INTO  x_existe,
                                        x_ind_edad,
                                        x_rechazo,
                                        x_rch_marca ,
                                        x_eje_transf,
                                        x_folio

         IF    x_rechazo = 0 THEN
               LET sw_aceptado       =  sw_aceptado     +  1
         ELSE
               LET sw_rechazado      =  sw_rechazado    +  1
         END IF

         EXECUTE p_ActGpo        USING  x_rechazo,
                                        x_folio,
                                        ls_intento,
                                        r.nss,
                                        r.grupo_regimen,
                                        r.siefore,
                                        r.siefore_ant,
                                        ld_fecha_proceso
      END FOREACH
      IF    sw_aceptado           >     0 THEN
            --- Genero al menos un aceptado
            LET ls_estado         =  3
      ELSE
            LET ls_estado         =  4
      END IF

      EXECUTE p_ActEdad          USING  ls_estado,
                                        vnss,
                                        ld_fecha_proceso
   END FOREACH

   CALL cambio_indicador_edad()

   LET lc_Proceso = "CTAB041"
   LET ls_Etapa = 2
   
   EXECUTE p_act_ctr_proc USING  lc_Proceso ,ls_Etapa

END FUNCTION

################################################################################
FUNCTION cambio_indicador_edad()

   DEFINE    vnss                  CHAR(11),
             sw_procesados         INTEGER ,
             vind_edad             SMALLINT

   DEFINE    r1 RECORD
             nss                   CHAR(11),
             ind_edad              SMALLINT,
             criterio              SMALLINT,
             estado                SMALLINT
   END RECORD

   DEFINE    x_existe              SMALLINT,
             x_ind_edad            SMALLINT,
             x_rechazo             SMALLINT,
             sw_aceptado           INTEGER,
             sw_rechazado          INTEGER
   DEFINE    ls_estado             SMALLINT

   LET     sw_procesados = 0
   LET     sw_aceptado   = 0
   LET     sw_rechazado  = 0

   FOREACH d_VerEdaGpo           USING  ld_fecha_proceso
                                  INTO  r1.nss,
                                        r1.ind_edad,
                                        r1.criterio,
                                        r1.estado
      LET sw_procesados = sw_procesados + 1

      EXECUTE   p_act_ind_eda    USING  r1.nss,             --- nss
                                        r1.ind_edad,        --- ind_edad
                                        r1.criterio         --- criterio
                                  INTO  x_rechazo
      IF x_rechazo = 1 THEN
           LET sw_aceptado           =  sw_aceptado  + 1
           IF  r1.estado             =  3     THEN 
               LET    ls_estado      =  5
           ELSE
               LET    ls_estado      =  7
           END IF
      ELSE
           LET sw_rechazado          =  sw_rechazado + 1
           IF  r1.estado             =  3     THEN
               LET    ls_estado      =  6
           ELSE
               LET    ls_estado      =  9
           END IF

      END IF
      EXECUTE p_ActEdad          USING  ls_estado,
                                        r1.nss,
                                        ld_fecha_proceso

   END FOREACH

END FUNCTION

