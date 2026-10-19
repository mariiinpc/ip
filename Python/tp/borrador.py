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



## ver el archivo de clase 19 e otubre labo 107

def generar_nros_al_azar(cantidad: int, desde: int, hasta: int) -> Cola[int]:
    num: Cola[int] = Cola()
    contador: int = 0
    while contador <= cantidad:
        num.put(random.randint(desde, hasta))
        contador +=1
    return num


def cola_desde_lista(l:list[int]) -> Cola[int]:
    res: Cola[int]= Cola()
    for i in l:
        res.put(i)
    return res


def armar_secuencia_de_bingo() -> Cola[int]:
    carton: Cola[int]= Cola()
    contador: int = 0
    while contador < 12:
        num = (random.randint(0, 99))
        if pertenece(carton, num) == False:
            carton.put(num)
        contador +=1
    return carton



def pertenece(c: Cola[int], n: int) -> bool:
    esta: bool= False
    vacio: bool = False
    while vacio == False:
        if c.get() == n:
            esta = True
            vacio = True
        vacio = c.empty()
    return esta



def movimiento_bien_formado(m: Movimiento) -> bool:
    mov = True
    if posicion_en_tablero(m[1]) and len(m[2]) == 1:
        if m[0] == MOVER_A_SECAS and m[2][0] == 1:
            mov = True
        elif m[0] == ARRASTRAR and m[2][0] == 2:
            mov = True
        elif m[0] == EMPUJAR and m[2][0] == 2:
            mov = True
        else:
            mov = False
    else:
        mov = False
    return mov


def es_posicion_vcina_hacia(pos: Posicion, d: Direccion) -> Posicion:
    nueva_pos= ()
    if d == NORTE:
        nueva_pos[0]
    
    
    
    
    #es_vecino = True
    #if d == NORTE and pos1[0] == pos[0] +1 and pos1[1] == pos[1]:
    #    es_vecino = True
    #elif d == SUR and pos1[0] == pos[0] -1 and pos1[1] == pos[1]:
    #    es_vecino = True
    #elif d == ESTE and pos1[0] == pos[0] and pos1[1] == pos[1]+1:
    #    es_vecino = True
    #elif d == OESTE and pos1[0] == pos[0] and pos1[1] == pos[1] -1:
    #    es_vecino = True
    #else:
    #    es_vecino = False
    #return es_vecino

def hay_casilla_vacia(t: Tablero, pos: Posición) -> bool:
    casilla_vacia = True
    if posicion_en_tablero(pos):
        if t[pos[0]][pos[1]] == VACIA:
            casilla_vacia = True
        else:
            casilla_vacia = False
    else:
        casilla_vacia = False
    return casilla_vacia

def armar_secuencia_de_bingo1() -> Cola[int]:
    carton: Cola[int]= Cola()
    contador: int = 0
    numeros_usados: list[int] = []
    while contador < 12:
        num = (random.randint(0, 99))
        if num not in numeros_usados:
            numeros_usados.append(num)
            contador +=1
            carton.put(num)
    return carton
        

def jugar_carton_de_bingo(carton: list[int], bolillero: Cola[int]) -> int:
    contador: int = 0
    while len(carton) > 0 and not bolillero.empty():
        numero_sacado: int = bolillero.get()
        contador += 1
        if numero_sacado in carton:
                carton.remove(numero_sacado)
    return contador



