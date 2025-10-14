--CARGO
INSERT INTO dis_cuenta VALUES(
264                         ,--tipo_movimiento
10                          ,--subcuenta
1                           ,--siefore
37762                       ,--folio
0                           ,--consecutivo_lote
'I0137711751'               ,--nss
''                          ,--curp
"0"                         ,--folio_sua
'12/01/2014'                ,--fecha_pago
'12/01/2014'                ,--fecha_valor
'12/01/2014'                ,--fecha_conversion
-31.724381                  ,--monto_en_pesos
-19.163530                  ,--monto_en_acciones
1.655456                    ,--precio_accion
0                           ,--dias_cotizados
""                          ,--sucursal
"op_11"                     ,--id_aportante
0                           ,--estado
'12/01/2014'                ,--fecha_proceso
USER                        ,--usuario
'12/01/2014'                ,--fecha_archivo
0                            --etiqueta
)
;

INSERT INTO dis_cuenta VALUES(
264                         ,--tipo_movimiento
13                          ,--subcuenta
4                           ,--siefore
37762                       ,--folio
0                           ,--consecutivo_lote
'I0137711751'               ,--nss
''                          ,--curp
"0"                         ,--folio_sua
'12/01/2014'                ,--fecha_pago
'12/01/2014'                ,--fecha_valor
'12/01/2014'                ,--fecha_conversion
-26952.945527               ,--monto_en_pesos
-14566.049696               ,--monto_en_acciones
1.850395                    ,--precio_accion
0                           ,--dias_cotizados
""                          ,--sucursal
"op_11"                     ,--id_aportante
0                           ,--estado
'12/01/2014'                ,--fecha_proceso
USER                        ,--usuario
'12/01/2014'                ,--fecha_archivo
0                            --etiqueta
)
;

--ABONO
INSERT INTO dis_cuenta VALUES(
265                         ,--tipo_movimiento
10                          ,--subcuenta
1                           ,--siefore
37762                       ,--folio
0                           ,--consecutivo_lote
'I0137712239'               ,--nss
''                          ,--curp
"0"                         ,--folio_sua
'12/01/2014'                ,--fecha_pago
'12/01/2014'                ,--fecha_valor
'12/01/2014'                ,--fecha_conversion
31.724381                   ,--monto_en_pesos
19.163530                   ,--monto_en_acciones
1.655456                    ,--precio_accion
0                           ,--dias_cotizados
""                          ,--sucursal
"op_11"                     ,--id_aportante
0                           ,--estado
'12/01/2014'                ,--fecha_proceso
USER                        ,--usuario
'12/01/2014'                ,--fecha_archivo
0                            --etiqueta
)
;

INSERT INTO dis_cuenta VALUES(
265                         ,--tipo_movimiento
13                          ,--subcuenta
4                           ,--siefore
37762                       ,--folio
0                           ,--consecutivo_lote
'I0137712239'               ,--nss
''                          ,--curp
"0"                         ,--folio_sua
'12/01/2014'                ,--fecha_pago
'12/01/2014'                ,--fecha_valor
'12/01/2014'                ,--fecha_conversion
26952.945527                ,--monto_en_pesos
14566.049696                ,--monto_en_acciones
1.850395                    ,--precio_accion
0                           ,--dias_cotizados
""                          ,--sucursal
"op_11"                     ,--id_aportante
0                           ,--estado
'12/01/2014'                ,--fecha_proceso
USER                        ,--usuario
'12/01/2014'                ,--fecha_archivo
0                            --etiqueta
)
; 

EXECUTE PROCEDURE marca_cuenta('I0137711751',130,0,0,0,0,null,user); 