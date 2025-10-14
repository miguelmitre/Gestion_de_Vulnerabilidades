{=========================================================================
Clave: 
Nombre: mConsulta.4gl
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Pantallas de Consulta para Solicitudes de Reinversion
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
DATABASE safre_af

GLOBALS
DEFINE
garr_tipoorigen ARRAY [100] OF RECORD
id_tipo_origen_reinv  LIKE ret_reinversion.id_tipo_origen_reinv, 
desc_origen_reinv     LIKE ret_tipo_origen_reinv.desc_origen_reinv
END RECORD,

garr_DescOrigen ARRAY [100] OF RECORD
  id_tipo_orig     LIKE ret_reinversion.id_tipo_origen_reinv
END RECORD,

garr_edoSolic ARRAY [100] OF RECORD
estado_solicitud  LIKE ret_reinversion.estado_solicitud, 
desc_nivel_reinv  LIKE ret_nivel_reinv.desc_nivel_reinv
END RECORD,

garr_consultaDatos ARRAY [100] OF RECORD 
 nss                  LIKE ret_solicitud_saldo.nss,
 fecha_registro       DATE,
 desc_origen_reinv    LIKE ret_tipo_origen_reinv.desc_origen_reinv,
 desc_nivel_reinv     LIKE ret_nivel_reinv.desc_nivel_reinv,
 estado_solicitud     LIKE ret_nivel_reinv.desc_nivel_reinv
END RECORD,
gi_tamArr       INTEGER,
QryTxt          CHAR(1000)
END GLOBALS

DEFINE ld_fecHoy DATE

MAIN
  OPTIONS
	  PROMPT LINE LAST,
	  INPUT WRAP,
	  ACCEPT KEY CONTROL-O
  
  DEFER INTERRUPT
  
  CALL fInicializa()
END MAIN

{=========================================================================
Clave: 
Nombre: fInicializa()
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Pantallas de Consulta para Solicitudes de Reinversion
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fInicializa()

 OPEN WINDOW v_Consulta AT 2,2 WITH FORM "RETC7641" ATTRIBUTE(BORDER)  
    LET ld_fecHoy = TODAY
    DISPLAY " <Ctrl-C> SALIR         <Esc> EJECUTAR                        GENERAL RETIRO " AT 1,1 ATTRIBUTE (REVERSE)
    DISPLAY " RETC764          CONSULTA DE SOLICITUDES DE REINVERSION                     " AT 3,1 ATTRIBUTES (REVERSE)
    DISPLAY "", ld_fecHoy USING "DD-MM-YYYY" AT 3,67 ATTRIBUTES (REVERSE)
    
      -- captura parametos de consulta
      CALL fCaptura_Param()
 
      --ON KEY (ESC)
      --   CALL fMuestraResultados()
       

CLOSE WINDOW v_Consulta
END FUNCTION 

{=========================================================================
Clave: 
Nombre: fCaptura_Param()
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Pantallas de Consulta para Solicitudes de Reinversion
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fCaptura_Param()

DEFINE 
  lr_captura RECORD 
  nss                  LIKE ret_solicitud_saldo.nss,
  fecha_registro       DATE,
  fecha_registro1      DATE,
  id_tipo_origen_reinv LIKE ret_reinversion.id_tipo_origen_reinv,
  desc_origen_reinv    LIKE ret_tipo_origen_reinv.desc_origen_reinv,
  estado_solicitud     LIKE ret_reinversion.estado_solicitud,
  desc_nivel_reinv     LIKE ret_nivel_reinv.desc_nivel_reinv
  END RECORD,
  li_indice               INTEGER,
  ls_id_tipo_origen_reinv SMALLINT,
  lc_desc_origen_reinv    CHAR(40),
  ls_estado_solicitud     SMALLINT,
  lc_desc_nivel_reinv     CHAR(40),
  lb_continuar            SMALLINT
 
  INITIALIZE lr_captura.* TO NULL


  -- se asume que se continua
  LET lb_continuar = 1

  WHILE ( lb_continuar = 1 )

  INPUT BY NAME lr_captura.nss, lr_captura.fecha_registro, lr_captura.fecha_registro1,
                lr_captura.id_tipo_origen_reinv, lr_captura.estado_solicitud
  --WITHOUT DEFAULTS
   ON KEY (control-c)
      LET lb_continuar = 0
      EXIT INPUT

    AFTER INPUT
      LET lb_continuar = 0

    ON KEY (CONTROL-W)
      -- si esta en el campo tipo origen
      IF ( INFIELD(id_tipo_origen_reinv) ) THEN
        OPEN WINDOW v_ConsultaOrigen AT 5,10 WITH FORM "RETC7642"
          
          CALL fObtieneOrigen() RETURNING gi_tamArr
          CALL  SET_COUNT(gi_tamArr - 1)    
        
            DISPLAY ARRAY garr_tipoorigen TO sa_diag.*
            
            ON KEY (Esc)
               LET li_indice = ARR_CURR()
               LET lr_captura.id_tipo_origen_reinv = garr_tipoorigen[li_indice].id_tipo_origen_reinv
               LET ls_id_tipo_origen_reinv = lr_captura.id_tipo_origen_reinv
               LET lr_captura.desc_origen_reinv = garr_tipoorigen[li_indice].desc_origen_reinv
               LET lc_desc_origen_reinv = lr_captura.desc_origen_reinv
               EXIT DISPLAY          
        
            ON KEY (CONTROL-C)
               --LET gi_tamArr = ARR_CURR()
               EXIT DISPLAY
            END DISPLAY
        CLOSE WINDOW v_ConsultaOrigen
        
        DISPLAY lr_captura.id_tipo_origen_reinv TO id_tipo_origen_reinv
        DISPLAY lr_captura.desc_origen_reinv TO desc_origen_reinv
      END IF
      
      -- si se esta en estado
      IF ( INFIELD(estado_solicitud) ) THEN
        OPEN WINDOW v_ConsultaEstado AT 5,10 WITH FORM "RETC7644"
          
          CALL fObtieneEstado() RETURNING gi_tamArr
          CALL  SET_COUNT(gi_tamArr - 1)    
        
            DISPLAY ARRAY garr_edoSolic TO sa_diag.*
                     
            ON KEY (Esc)
               LET li_indice = ARR_CURR()
               LET lr_captura.estado_solicitud = garr_edoSolic[li_indice].estado_solicitud
               LET lr_captura.desc_nivel_reinv = garr_edoSolic[li_indice].desc_nivel_reinv
               EXIT DISPLAY
        
            ON KEY (CONTROL-C)
               --LET gi_tamArr = ARR_CURR()
               EXIT DISPLAY
            END DISPLAY
        CLOSE WINDOW v_ConsultaEstado
        
        DISPLAY lr_captura.estado_solicitud TO estado_solicitud
        DISPLAY lr_captura.desc_nivel_reinv TO desc_nivel_reinv   
      END IF
     
    -- despues de capturar la clave de tipo de origen
    AFTER FIELD id_tipo_origen_reinv
      IF ( lr_captura.id_tipo_origen_reinv IS NOT NULL ) THEN
        -- se busca la descripcion de la clave capturada
        CALL fDescripcionOrigen(lr_captura.id_tipo_origen_reinv) RETURNING lr_captura.desc_origen_reinv
        IF ( lr_captura.desc_origen_reinv IS NULL ) THEN
          -- la clave no existe, la debe cpaturar de nuevo
          ERROR "LA CLAVE DE TIPO DE ORIGEN DE REINVERSION NO EXISTE"
          NEXT FIELD id_tipo_origen_reinv
        ELSE
          DISPLAY lr_captura.desc_origen_reinv TO desc_origen_reinv
        END IF
      END IF

    -- despues de capturar la clave de tipo de origen
    AFTER FIELD estado_solicitud
      IF ( lr_captura.estado_solicitud IS NOT NULL ) THEN
        -- se busca la descripcion de la clave capturada
        CALL fDescripcionEstado(lr_captura.estado_solicitud) RETURNING lr_captura.desc_nivel_reinv
        IF ( lr_captura.desc_nivel_reinv IS NULL ) THEN
          -- la clave no existe, la debe cpaturar de nuevo
          ERROR "LA CLAVE DE ESTADO DE SOLICITUD NO EXISTE"
          NEXT FIELD estado_solicitud
        ELSE
          DISPLAY lr_captura.desc_nivel_reinv TO desc_nivel_reinv
        END IF
      END IF
     
	  ON KEY (Esc)
	    -- se invoca la funcion para traer los datos en buffer
      LET lr_captura.nss                  = GET_FLDBUF(nss) 
      LET lr_captura.fecha_registro       = GET_FLDBUF(fecha_registro)
      LET lr_captura.fecha_registro1      = GET_FLDBUF(fecha_registro1)
      LET lr_captura.id_tipo_origen_reinv = GET_FLDBUF(id_tipo_origen_reinv)
      --LET lr_captura.desc_origen_reinv    = GET_FLDBUF(desc_origen_reinv)
      LET lr_captura.estado_solicitud     = GET_FLDBUF(estado_solicitud)
      --LET lr_captura.desc_nivel_reinv     = GET_FLDBUF(desc_nivel_reinv)
      --DISPLAY "r_cons ", lr_captura.* SLEEP 2
      -- si no se capturan parametros, se le indica al usuario
      IF ( (lr_captura.nss     CLIPPED      IS NULL) AND
           (lr_captura.fecha_registro       IS NULL) AND
           (lr_captura.fecha_registro1      IS NULL) AND
           (lr_captura.id_tipo_origen_reinv IS NULL) AND
           (lr_captura.estado_solicitud     IS NULL) ) THEN
        ERROR "DEBE CAPTURAR AL MENOS UN PARAMETRO DE CONSULTA"
        NEXT FIELD nss
      END IF

      
      -- si se captura una fecha, deben estar las dos
      IF ( lr_captura.fecha_registro IS NOT NULL ) THEN
        IF ( lr_captura.fecha_registro1 IS NULL ) THEN
          ERROR "SI CAPTURA FECHA DE REGISTRO, DEBE INDICAR LA FECHA FINAL"
          NEXT FIELD fecha_registro1
        END IF
      END IF

      IF ( lr_captura.fecha_registro1 IS NOT NULL ) THEN
        IF ( lr_captura.fecha_registro IS NULL ) THEN
          ERROR "SI CAPTURA FECHA DE REGISTRO, DEBE INDICAR LA FECHA INICIAL"
          NEXT FIELD fecha_registro
        END IF
      END IF
    
      -- se envian los parametros de consulta a la funcion que despliega los registros
      -- encontrados en pantalla
      CALL fConsultaDatos(lr_captura.*)
 
      CLEAR FORM
      
      INITIALIZE lr_captura.* TO NULL
      EXIT INPUT        

   END INPUT 
   
 END WHILE
END FUNCTION 


{=========================================================================
Clave: 
Nombre: fObtieneOrigen()
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Funcion que recuperta los datos de tipo de origen 
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fObtieneOrigen()

DEFINE
 lr_tipoorigen   RECORD 
  id_tipo_orig     LIKE ret_reinversion.id_tipo_origen_reinv,
  des_tipo_ori     LIKE ret_tipo_origen_reinv.desc_origen_reinv
 END RECORD, 
 li_indice      INTEGER
 
 WHENEVER ERROR CONTINUE
 
  LET li_indice = 1
 
  LET QryTxt = "\n SELECT id_tipo_origen_reinv, desc_origen_reinv",
               "\n   FROM  ret_tipo_origen_reinv",
               "\n  ORDER BY id_tipo_origen_reinv"

   PREPARE prp_TipoOrigen FROM QryTxt
   DECLARE cur_TipoOrigen CURSOR FOR prp_TipoOrigen
 
 
   INITIALIZE  garr_tipoorigen TO NULL
 
     FOREACH cur_TipoOrigen INTO lr_tipoorigen.*
       LET garr_tipoorigen[li_indice].id_tipo_origen_reinv   = lr_tipoorigen.id_tipo_orig
       LET garr_tipoorigen[li_indice].desc_origen_reinv = lr_tipoorigen.des_tipo_ori
       LET li_indice = li_indice + 1
     END FOREACH
 
 FREE cur_TipoOrigen
 
 RETURN li_indice
END FUNCTION





{=========================================================================
Clave: 
Nombre: fObtieneEstado()
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Funcion que recuperta los datos del estado de la solicitud
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fObtieneEstado()
DEFINE
 lr_edoSolic   RECORD 
  edo_solicitud     LIKE ret_reinversion.estado_solicitud, 
  desc_edo_solic     LIKE ret_nivel_reinv.desc_nivel_reinv
 END RECORD, 
 li_indice      INTEGER
 
  
 WHENEVER ERROR CONTINUE
 
  LET li_indice = 1
 
  LET QryTxt = "\n SELECT *",
               "\n   FROM ret_estado",
               "\n  WHERE estado_solicitud",
               "\n     IN (140, 142, 144, 146)",               
               "\n  ORDER BY estado_solicitud"

   PREPARE prp_edoSolicitud FROM QryTxt
   DECLARE cur_edoSolicitud CURSOR FOR prp_edoSolicitud
   
   
   INITIALIZE  garr_edoSolic TO NULL
   
   FOREACH cur_edoSolicitud INTO lr_edoSolic.*
     LET garr_edoSolic[li_indice].estado_solicitud = lr_edoSolic.edo_solicitud 
     LET garr_edoSolic[li_indice].desc_nivel_reinv = lr_edoSolic.desc_edo_solic
     LET li_indice = li_indice + 1
   END FOREACH
   
 FREE cur_edoSolicitud
 
 RETURN li_indice
END FUNCTION

{=========================================================================
Clave: 
Nombre: fConsultaDatos()
Fecha Creación: 09/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Funcion que recuperta los datos de tipo de origen 
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fConsultaDatos(lr_consulta)

DEFINE
 lr_consulta   RECORD
  nss            LIKE ret_solicitud_saldo.nss,
  fecha_ini      LIKE ret_reinversion.fecha_registro,
  fecha_fin      LIKE ret_reinversion.fecha_registro,
  id_tipo_orig   LIKE ret_reinversion.id_tipo_origen_reinv,
  des_tipo_ori   LIKE ret_tipo_origen_reinv.desc_origen_reinv,
  edo_solicitud  LIKE ret_reinversion.estado_solicitud, 
  desc_edo_solic LIKE ret_nivel_reinv.desc_nivel_reinv
 END RECORD, 
 lr_resultado_consulta RECORD
  nss                   LIKE ret_solicitud_saldo.nss, -- numero de seguridad social
  fecha_registro        DATE, -- fecha de registro
  tipo_origen_reinv     VARCHAR(255), -- tipo de origen con descripcion
  des_nivel_reinv       VARCHAR(255), -- nivel de reinversion con descripcion
  desc_edo_solic        VARCHAR(255) -- estado de solicitud con descripcion
 END RECORD, 
 lv_total_solicitudes  VARCHAR(255),
 li_indice      INTEGER,
 fecha_dia      DATE,
 lb_continuar            SMALLINT
 
 -- se obtiene la fecha del dia
 LET fecha_dia = TODAY
 

{DISPLAY "LR NSS ", lr_consulta.nss
DISPLAY "F INI ", lr_consulta.fecha_ini
DISPLAY "F FIN ", lr_consulta.fecha_fin
DISPLAY "ID ORIG", lr_consulta.id_tipo_orig
DISPLAY "ID EDO", lr_consulta.edo_solicitud
}

  --WHENEVER ERROR CONTINUE
  --DISPLAY "llego nss: '", lr_consulta.nss CLIPPED, "'"
  LET li_indice = 1
 
  LET QryTxt = "\n SELECT rs.nss, rr.fecha_registro,",
               "\n        rr.id_tipo_origen_reinv || '-' || rt.desc_origen_reinv AS ORIGEN,",
               "\n        rt.id_nivel_reinv || '-' || rn.desc_nivel_reinv AS NIVEL_REINV,",
               "\n        rr.estado_solicitud || '-' || re.descripcion AS ESTADO",
               "\n   FROM ret_solicitud_saldo rs JOIN ret_reinversion rr",
               "\n     ON rr.id_solicitud_saldo = rs.id_solicitud_saldo",
               "\n   JOIN ret_tipo_origen_reinv rt ",
               "\n     ON rt.id_tipo_origen_reinv = rr.id_tipo_origen_reinv",
               "\n   JOIN ret_nivel_reinv rn",
               "\n     ON rn.id_nivel_reinv = rt.id_nivel_reinv",
               "\n   JOIN ret_estado re",
               "\n     ON re.estado_solicitud = rr.estado_solicitud",
               "\n   WHERE rs.estado_solicitud IN (106,120) " 
##Se agrega estado 120 = Reinvertido ACS ENE2012

{
               "\n  WHERE rs.nss =", lr_consulta.nss,
               "\n     OR rr.fecha_registro BETWEEN", lr_consulta.fecha_ini, "AND" ,lr_consulta.fecha_fin,
               "\n     OR rr.id_tipo_origen_reinv = ",lr_consulta.id_tipo_orig,
               "\n     OR rr.estado_solicitud = ",lr_consulta.edo_solicitud,
               "\n  ORDER BY 1"
}  
  

  -- se concatenan los filtros si es que se tienen
  IF ( lr_consulta.nss CLIPPED IS NOT NULL ) THEN
    LET QryTxt = QryTxt CLIPPED || "\nAND rs.nss = '" || lr_consulta.nss CLIPPED || "'"
  END IF

  IF ( lr_consulta.fecha_ini IS NOT NULL ) THEN
    LET QryTxt = QryTxt CLIPPED || "\nAND rr.fecha_registro BETWEEN '" || lr_consulta.fecha_ini || "' AND '" || lr_consulta.fecha_fin ||"'"
  END IF
    
  IF ( lr_consulta.id_tipo_orig   IS NOT NULL ) THEN
    LET QryTxt = QryTxt CLIPPED || "\nAND rr.id_tipo_origen_reinv = " || lr_consulta.id_tipo_orig
  END IF
  
  IF ( lr_consulta.edo_solicitud  IS NOT NULL ) THEN
    LET QryTxt = QryTxt CLIPPED || "\nAND rr.estado_solicitud = " || lr_consulta.edo_solicitud
  END IF

  LET QryTxt = QryTxt CLIPPED || "\nORDER BY 1"

--  DISPLAY QryTxt
--  SLEEP 2

  PREPARE prp_ConsultaDatos FROM QryTxt
  DECLARE cur_ConsultaDatos CURSOR FOR prp_ConsultaDatos
 
  FOREACH cur_ConsultaDatos INTO lr_resultado_consulta.*
    -- se asignan los datos al arreglo de despliegue
    LET garr_consultaDatos[li_indice].nss               = lr_resultado_consulta.nss
    LET garr_consultaDatos[li_indice].fecha_registro    = lr_resultado_consulta.fecha_registro
    LET garr_consultaDatos[li_indice].desc_origen_reinv = lr_resultado_consulta.tipo_origen_reinv
    LET garr_consultaDatos[li_indice].desc_nivel_reinv  = lr_resultado_consulta.des_nivel_reinv
    LET garr_consultaDatos[li_indice].estado_solicitud  = lr_resultado_consulta.desc_edo_solic
    
    -- se incrementa el indice
    LET li_indice = li_indice + 1
  END FOREACH
  
  FREE cur_ConsultaDatos
 
  -- numero de registros encontrados
  CALL SET_COUNT(li_indice - 1)
 
  -- si no encontro registros   indica error de que no encontro registros, y regresa a la pantalla anterior
  IF ( li_indice - 1 < 1 ) THEN
     ERROR "LOS PARAMETROS DE CONSULTA NO GENERARON REGISTROS"
    RETURN
  END IF
 
  OPEN WINDOW v_Resultados AT 2,2 WITH FORM "RETC7643" ATTRIBUTES (BORDER)
  DISPLAY " <CTRL-C> SALIR                                                GENERAL RETIRO " AT 1,1 ATTRIBUTE(REVERSE)
  DISPLAY " RETC764            CONSULTA DE SOLICITUDES DE REINVERSION                  " AT 3,1 ATTRIBUTE(REVERSE)
  DISPLAY fecha_dia USING "DD-MM-YYYY" AT 3,69 ATTRIBUTE(REVERSE)

  LET li_indice = li_indice - 1
  LET lv_total_solicitudes = "TOTAL DE SOLICITUDES: " || li_indice
  ERROR lv_total_solicitudes
  
  DISPLAY ARRAY garr_consultaDatos TO sa_result.*
  
    ON KEY (CONTROL-C)
    LET lb_continuar = 0
      EXIT DISPLAY
      
  END DISPLAY
    
  CLOSE WINDOW v_Resultados

END FUNCTION

{=========================================================================
Clave: 
Nombre: fDescripcionOrigen()
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Funcion que recuperta los datos de tipo de origen 
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fDescripcionOrigen(ls_idorigen)
DEFINE
 ls_idorigen    SMALLINT,
 lv_descripcion VARCHAR(255)
 
 WHENEVER ERROR CONTINUE
 
 {
  LET QryTxt = "\n SELECT desc_origen_reinv",
               "\n   FROM  ret_tipo_origen_reinv",
               "\n  WHERE ls_idorigen =", ls_idorigen,
               "\n  ORDER BY id_tipo_origen_reinv"}

   SELECT desc_origen_reinv
   INTO lv_descripcion
   FROM  ret_tipo_origen_reinv
   WHERE id_tipo_origen_reINV = ls_idorigen

{
   PREPARE prp_DescOrigen FROM QryTxt
   DECLARE cur_DescOrigen CURSOR FOR prp_DescOrigen
 }
 
 RETURN lv_descripcion
END FUNCTION

{=========================================================================
Clave: 
Nombre: fDescripcionEstado()
Fecha Creación: 08/12/2011
Autor: Antonio Gómez -UPGENIA
Narrativa Del Proceso que Realiza:
 Busca la descripcion de una clave de estado de solicitud
Parametros: Entrada: 
            Salida:
Registro de Modificaciones:
Autor           Fecha                   Descrip. cambio
=========================================================================}
FUNCTION fDescripcionEstado(lv_edo_solicitud)
DEFINE
 lv_edo_solicitud     LIKE ret_reinversion.estado_solicitud,
 lv_descripcion VARCHAR(255)
 
 WHENEVER ERROR CONTINUE
 
 {
  LET QryTxt = "\n SELECT desc_origen_reinv",
               "\n   FROM  ret_tipo_origen_reinv",
               "\n  WHERE ls_idorigen =", ls_idorigen,
               "\n  ORDER BY id_tipo_origen_reinv"}

   SELECT descripcion
     INTO lv_descripcion
     FROM  ret_estado
    WHERE estado_solicitud = lv_edo_solicitud

{
   PREPARE prp_DescOrigen FROM QryTxt
   DECLARE cur_DescOrigen CURSOR FOR prp_DescOrigen
 }
 
 RETURN lv_descripcion
END FUNCTION