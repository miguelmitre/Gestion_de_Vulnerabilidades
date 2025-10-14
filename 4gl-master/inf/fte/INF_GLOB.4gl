################################################################################
# Proyecto          => SISTEMA DE AFORES( MEXICO )                             #
# Sistema           => INF                                                     #
# Programa INF_GLOB => UTILERIAS DE USO GENERAL PARA LOS PROGRAMAS DEL MODULO  #
#                      DE INFONAVIT.                                           #
# Creado por        => ISAI JIMENEZ ROJAS                                      #
# Fecha creacion    => SEPTIEMBRE DEL 2004                                     #
# Actualizacion     => ISAI JIMENEZ ROJAS                                      #
# fecha actualiz.   => 23 DE SEPTIEMBRE 04                                     #
#                   =>                                                         #
################################################################################
DATABASE safre_af

-- Declaracion de Variables Modulares
DEFINE ma_tab_tipo_devol ARRAY[100] OF RECORD LIKE tab_tipo_devol.*

#========================================================================
-- Ayuda para el campo tipo de devolucion 
-- tabla: tab_tipo_devol
#========================================================================

FUNCTION z_tab_tipo_devol()
   DEFINE vnum_regs SMALLINT
   DEFINE vpos_arr  SMALLINT

   CALL carga_tab_tipo_devol() RETURNING vnum_regs

   IF vnum_regs = 0 THEN
      RETURN FALSE
   END IF

   OPEN WINDOW w1 AT 5,12 WITH FORM "INF_GLOB1"
   ATTRIBUTE(FORM LINE 1, BORDER)

   CALL SET_COUNT(vnum_regs)

   DISPLAY ARRAY ma_tab_tipo_devol TO sa_tab_tipo_devol.*
      ON KEY(ESC)
         LET vpos_arr = ARR_CURR()
         EXIT DISPLAY
   END DISPLAY

   IF INT_FLAG = TRUE THEN
      LET INT_FLAG = FALSE
      LET vpos_arr = 1
      INITIALIZE ma_tab_tipo_devol[vpos_arr].* TO NULL
   END IF

   CLOSE WINDOW w1

   RETURN ma_tab_tipo_devol[vpos_arr].*

END FUNCTION

----------------------------------------------------------------
-- Ibjetivo: Cargar los registros de la tabla en el arreglo
-- Devuelve: Numero de registros cargados en el arreglo
----------------------------------------------------------------
FUNCTION carga_tab_tipo_devol()
   DEFINE indice            SMALLINT

   DECLARE cur_tab_tipo_devol CURSOR FOR
     SELECT * FROM tab_tipo_devol

   LET indice = 1

   FOREACH cur_tab_tipo_devol INTO ma_tab_tipo_devol[indice].*

      LET indice = indice + 1

   END FOREACH

   LET indice = indice -1

   RETURN indice -- regresa el No de Regs. cargados

END FUNCTION

#========================================================================#
# Objetivo: Despliega en una ventana el mensaje recibido como parametro  #
#           haciendo una pausa                                           #
#========================================================================#

FUNCTION notifica(mensaje)
   DEFINE mensaje   CHAR(70)
   DEFINE tam_cad   SMALLINT
   DEFINE tam_col   SMALLINT
   DEFINE pos_col   SMALLINT
   DEFINE tecla     CHAR(1)

   LET tam_cad = LENGTH(mensaje)
   LET pos_col = (78 - tam_cad)/2
   LET tam_col = tam_cad + 4

   OPEN WINDOW w_notifica AT 10, pos_col WITH 4 ROWS, tam_col COLUMNS
        ATTRIBUTE (BORDER,PROMPT LINE LAST)
     DISPLAY mensaje AT 2,3

     PROMPT "PRESIONE <RETURN> : " FOR CHAR tecla 

   CLOSE WINDOW w_notifica

END FUNCTION

--=======================================================================
-- OBTIENE EL PRECIO DE LA ACCION DEL DIA Y LA SIEFORE RECIBIDOS
--=======================================================================

FUNCTION precio_del_dia(vfecha,vcodigo_siefore)

   DEFINE vfecha           DATE
   DEFINE vcodigo_siefore  LIKE glo_valor_accion.codigo_siefore
   DEFINE vprecio_del_dia  LIKE glo_valor_accion.precio_del_dia

   SELECT TRUNC(precio_del_dia,6)
     INTO vprecio_del_dia
     FROM glo_valor_accion
    WHERE fecha_valuacion = vfecha
      AND codigo_siefore = vcodigo_siefore

   RETURN vprecio_del_dia

END FUNCTION

