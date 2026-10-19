def tablero_bien_formado(t : Tablero) -> bool:
    es_tablero: bool = True
    if len(t) == ALTO:                          #lo copie de aye pero con lo del ancho y alto por si acaso
        for fila in t:
            if len(fila) != ANCHO:
                es_tablero = False
    return es_tablero

def posicion_en_tablero(pos : Posicion) -> bool:
    es_posicion: bool = True
    if 0 <= pos[0] and pos[0]< ALTO:
        if 0<= pos[1] and pos[1]<ANCHO:
            es_posicion = True
        else:
            es_posicion = False
    else: 
        es_posicion = False
    return es_posicion




def movimiento_bien_formado(m: Movimiento) -> bool:
    mov = True
    if posicion_en_tablero(m[1]:
        if m[0] = MOVER_A_SECAS:
          if len(m[2]) == 1:
            for i in m[2]:
                if i == 1:
                        mov = True
                else:
                        mov = False
          else: 
            mov = False
        elif m[0] = ARRASTRAR:
          if len(m[2]) == 1:
            for i in m[2]:
                if i == 2:
                        mov = True
                else:
                        mov = False
          else:
            mov = False
        elif m[0] = EMPUJAR
            if len(m[2]) == 1:
                for i in m[2]:
                    if i == 2:
                        mov = True
                    else:
                        mov = False
            else: 
                mov = False
        else:
            mov = False
    else:
        mov = False
return mov


