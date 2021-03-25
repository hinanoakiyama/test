open UniverseJs
open Color
open Image
open World
open TransformToInt

type world_t = {
  field : int * int;
  player : int * int;
  mukijump : int * int;
  kasoku : int;
  kuri : (int * int) * int;
  kuricount : int;
}
let initial_world = {field = (250, 350); player = (30, 200); mukijump = (0, 1); kasoku = -5; kuri = ((400, 310), 0); kuricount = 0}
let width = 500
let height = 400
let pwidth = 50
let pheight = 50
let kwidth = 40
let kheight = 40
let klimit = 50
let field = read_image "./image/feald.png" 500 100
let player0 = read_image "./image/mario4.png" pwidth pheight
let player1 = read_image "./image/mario6.png" pwidth pwidth
let player02 = read_image "./image/mario2.png" pwidth pheight
let player12 = read_image "./image/mario7.png" pwidth pheight
let player03 = read_image "./image/mario2.png" pwidth pheight
let player13 = read_image "./image/mario7.png" pwidth pheight
let kuri1 = read_image "./image/kuri4.png" kwidth kheight
let kuri2 = read_image "./image/kuri5.png" kwidth kheight
let kuri3 = read_image "./image/kuri6.png" kwidth (kheight / 2)
let draw {field = (fx, fy); player = (px, py); mukijump = (muki, jump); kasoku = kasoku; kuri = ((kx, ky), k); kuricount = kcount} =
  place_image (if muki = 0 then (if jump = 1 then player02
  else player0)
  else if jump = 1 then player12
  else player1) (px, py) (place_image (if k = 0 then kuri1
  else if k = 1 then kuri2
  else kuri3) (if k = 3 then (kx, ky + 10)
  else (kx, ky)) (place_image field (fx, fy) (empty_scene width height)))
let on_key {field = (fx, fy); player = (px, py); mukijump = (muki, jump); kasoku = kasoku; kuri = ((kx, ky), k); kuricount = kcount} key =
  if key = "left" then {field = (fx, fy); player = (px - 10, py); mukijump = (1, jump); kasoku = kasoku; kuri = ((kx, ky), k); kuricount = kcount}
  else if key = "right" then {field = (fx, fy); player = (px + 10, py); mukijump = (0, jump); kasoku = kasoku; kuri = ((kx, ky), k); kuricount = kcount}
  else if key = "up" then {field = (fx, fy); player = (px, py); mukijump = (muki, 1); kasoku = 50; kuri = ((kx, ky), k); kuricount = kcount}
  else {field = (fx, fy); player = (px, py); mukijump = (muki, jump); kasoku = kasoku; kuri = ((kx, ky), k); kuricount = kcount}
let on_tick {field = (fx, fy); player = (px, py); mukijump = (muki, jump); kasoku = kasoku; kuri = ((kx, ky), k); kuricount = kcount} =
  {field = (fx, fy); player = (px, (if jump = 1 then py - kasoku
  else if py < fy - 55 then py - kasoku
  else fy - 45)); mukijump = (muki, (if py < fy - 60 then 1
  else 0)); kasoku = (if py < fy - 50 then (if kasoku < -10 then kasoku
  else kasoku - 5)
  else 0); kuri = ((if k = 3 then (if klimit > kcount then (kx, ky)
  else (400, 310))
  else (kx - 1, ky)), (if klimit > kcount then (if kcount > 0 then 3
  else (if jump = 0 then (if k = 0 then 1
  else if k = 1 then 0
  else 3)
  else if kx + 40 > px && px > kx - 40 && py > ky - 20 && kasoku <= 0 then 3
  else (if k = 0 then 1
  else 0)))
  else 0)); kuricount = (if k = 3 then (if klimit > kcount then kcount + 1
  else 0)
  else 0)}
;; big_bang initial_world
  ~width:width
  ~height:height
  ~to_draw:draw
  ~on_tick:on_tick
  ~on_key_press:on_key
  ~rate:100