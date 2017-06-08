(** Get a random coefficient in [-B, B]. *)
let sample_coefficient bound =
  let c = Random.int (bound + 1) in
  if Random.bool ()
  then Int.of_int c
  else Int.of_int (-c)

(** Get a random `bound`-bounded polynomial of degree at most degree. *)
let sample_bounded_poly degree bound =
  let coeff _ = sample_coefficient bound in
  Polynomial.make (Array.init degree coeff)

(* Type for floats in [0, 1] representing probabilities. *)
type probability = float

(* The PDF for a Gaussian distribution with standard deviation std_dev *)
let gaussian_pdf (std_dev : float) (x : float) : probability =
  let scaled_x = x /. std_dev in
  exp (-. (scaled_x *. scaled_x) /. 2.0)

(* The inverse of gaussian_pdf std_dev x assuming x > 0 *)
let gaussian_pdf_inverse (std_dev : float) (y : probability) : float =
  let l = log y in
  if l > 0.0
  then 0.0
  else std_dev *. sqrt(-2.0 *. l)

(** A rectangle in a ziggurat has upper and lower y-coordinates, a rightmost
    bound y_width, and a rejection point x_rej marking where the rectangle above
    it terminates. *)
type rectangle =
  { x_width : int
  ; x_rejection : int
  ; y_upper : probability
  ; y_lower : probability
  }

(* The weight of a rectangle is its height in probability times the number of
   integer x-coordinates in it. *)
let rect_weight rect : float =
  (rect.y_upper -. rect.y_lower) *. float_of_int (1 + rect.x_width)

type ziggurat_params =
  { zstd_dev : float
  ; ztail_cut : float
  ; zrectangle_count : int
  }

(* A ziggurat is a collection of rectangles specified by an array of corners, a
   standard deviation, and a tail cutoff *)
type ziggurat =
  { corners : (int * probability) array
  ; params : ziggurat_params
  }

let size_heuristic std_dev rectangle_count =
  let sqrtpi2 = sqrt (BatFloat.pi *. 0.5) in
  let count_float = float_of_int rectangle_count in
  std_dev *. sqrtpi2 /. count_float

let rho = gaussian_pdf

let compute_recurrence xis rhos b c m sigma =
  let ret = ref (-1.0) in
  let em = m + 1 in
  let om = 1.0 /. float_of_int m in
  let m2 = -2.0 in
  let o2 = 1.0 /. 2.0 in
  let area = sigma *. om *. sqrt (BatFloat.pi /. 2.0) *. c in
  xis.(em - 1) <- b;
  rhos.(em - 1) <- rho sigma (float_of_int (1 + int_of_float xis.(em - 1)));
  let sqrtv = m2 *. log (area /. (float_of_int (1 + int_of_float xis.(em - 1)))) in
  begin
    if sqrtv < 0.0
    then ret := -1.0
    else
      begin
        xis.(em - 2) <- sigma *. sqrt sqrtv;
        rhos.(em - 2) <- rho sigma xis.(em - 2);
        for i = em - 3 downto 0 do
          let sqrtv = m2 *. log (area /. (float_of_int (1 + int_of_float xis.(i + 1)))) in
          if sqrtv < 0.0
          then ret := -1.0
          else begin
              xis.(i) <- sigma *. sqrt sqrtv;
              rhos.(i) <- exp (-. o2 *. ((xis.(i) /. sigma) *. (xis.(i) /. sigma)))
            end
        done;
        rhos.(0) <- area /. (float_of_int (1 + int_of_float xis.(1))) +. rho sigma xis.(1);
        ret := rhos.(0)
      end
  end;
  !ret

let epsilon = 0.01

    (*
let improve_area st =
  let st' = ref {st with lastdiff = st.diff} in
  let xis = Array.make (st.m + 1) (-1.0) in
  let rhos = Array.make (st.m + 1) (-1.0) in
  let y0 = compute_recurrence xis rhos st.c st.m st.sigma in
  st' := {!st' with diff = y0 -. 1.0};
  if st'.diff < -1.0
  then Left st'
  else
    begin
      (if st'.diff >= 0.0
       then st' := {!st' with bestxis = xis; cu = st.c; bestdiff = !st'.diff}
       else st' := {!st' with cl = st.c});
      (if !st'.cu < !st'.cl
       then st' := {!st' with c = !st'.c +. 1.0 /. float_of_int !st'.m}
       else st' := {!st' with c = (!st'.cu +. !st'.cl) /. 2.0});
      let ret = !st' in
      if ret.c >= 11.0 || diff = ret.lastdiff
      then Left ret
      else Right (ret, ret.bestxis)
    end
*)

(* Given a std_dev, an initial value for x_m, a rectangle count, and an area for
   rectangles, try to solve the recurrences defining a ziggurat and return a
   list of corners. *)
let accumulate_corners std_dev x_m count area =
  let corners = Array.make (count + 1) (-1, -1.0) in

  let next_y (x, y) =
    area /. (1.0 +. float_of_int x) +. y
  in

  let next_corner (x, y) =
    let y' = next_y (x, y) in
    let x'_float = floor (gaussian_pdf_inverse std_dev y') in
    let x' = int_of_float x'_float in
    Some (x', gaussian_pdf std_dev x'_float)
  in

  let rec go i =
    if i > 0
    then match next_corner corners.(i+1) with
         | None -> None
         | Some (x', y') ->
            corners.(i) <- if i = 0
                          then (0, y')
                          else (x', y');
            go (i - 1)
    else
      begin
        if fst corners.(1) > 0
        then (corners.(0) <- (0, next_y corners.(1));
             Some corners)
        else None
      end

  in

  corners.(count) <- (x_m, 0.0);
  go (count - 1)

let random_sign () =
  if Random.bool ()
  then 1
  else -1

let rectangle_count zigg =
  (Array.length zigg.corners) - 1

let nth_rectangle zigg i =
  let (x_rejection, y_upper) = zigg.corners.(i) in
  let (x_width, y_lower) = zigg.corners.(i+1) in
  Printf.eprintf "\nx_rej = %d\nx_wid = %d\ny_upp = %f\ny_low = %f\n" x_rejection x_width y_upper y_lower;
  assert (x_width > 0);
  { x_width = x_width
  ; x_rejection = x_rejection
  ; y_lower = y_lower
  ; y_upper = y_upper
  }

let choose_rectangle zigg =
  let index = Random.int (rectangle_count zigg) in
  nth_rectangle zigg index

let random_y_coordinate rect =
  rect.y_lower +. Random.float (rect.y_upper -. rect.y_lower)

let rec sample_from_ziggurat zigg =
  (* choose rectangle, sign, and value *)
  assert (rectangle_count zigg > 2);
  let rect = choose_rectangle zigg in
  let sign = random_sign () in
  let x = Random.int rect.x_width in
  (* are we inside the curve? *)
  if 0 < x && x <= rect.x_rejection
  then sign * x
  else if x = 0
  then
    if Random.bool ()
    then 0
    else sample_from_ziggurat zigg
  else
    (* in rejection area *)
    let y = random_y_coordinate rect in
    let rhosigmax = gaussian_pdf zigg.params.zstd_dev (float_of_int x)  in
    if y < rhosigmax
    then x
    else sample_from_ziggurat zigg

type discrete_gaussian =
  { zigg : ziggurat
  ; dimension : int
  }

let good_enough diff last_diff =
  let diff_delta = abs_float (diff -. last_diff) in
  let diff_norm = abs_float diff in
  let valid = diff >= 0.0 in
  let converged_or_tight = diff_norm <= epsilon || diff_delta <= epsilon in
  valid && converged_or_tight

type ('a, 'p) objective_result =
  { score : float
  ; point : 'a
  ; params : 'p
  }

type ('a, 'p) objective_fn =
  'p -> (('a, 'p) objective_result) option

type partition_params =
  { (* fixed *)
    std_dev : float
  ; rects : int
  ; tail_cut : int
    (* variable *)
  ; bound : float
  ; size : float
  }

let make_partition params =
  let xis = Array.make (params.rects + 1) (-1.0) in
  let rhos = Array.make (params.rects + 1) (-1.0) in
  let y0 = compute_recurrence xis rhos params.bound params.size params.rects params.std_dev in
  let result = { score = y0 -. 1.0
               ; point = (xis, rhos)
               ; params = params
               }
  in
  if xis.(0) = -1.0
  then None
  else Some result

let linear_search epsilon persevere (objective : ('a, float) objective_fn) initial_params upper_bound increment =
  let negligible_improvement cur prev =
    match prev with
    | Some p ->
       abs_float (cur.score -. p.score) <= epsilon
    | None -> false
  in

  let rec go lower prev =
    if lower > upper_bound
    then None
    else
      let next = objective lower in
      match next with
      | None ->
         if persevere
         then go (lower +. increment) next
         else None
      | Some soln ->
         if soln.score >= 0.0
         then Some soln
         else if negligible_improvement soln prev
         then None
         else go (lower +. increment) next
  in
  go initial_params None

let size_heuristic sigma m =
  sigma /. (m *. sqrt(BatFloat.pi /. 2.0))

let option_map f opt =
  match opt with
  | Some x -> Some (f x)
  | None -> None

let find_good_partition std_dev rects tail_cut =
  let f_rects = float_of_int rects in
  let f_tail_cut = float_of_int tail_cut in

  let size_init = 1.0 +. 1.0 /. f_rects in
  let size_cutoff = (size_heuristic std_dev f_rects) /. (f_tail_cut *. std_dev +. 1.0) in
  let size_increment = 1.0 /. f_rects in

  let bound_init = std_dev *. f_tail_cut in
  let bound_cutoff = std_dev *. (1.0 +. float_of_int tail_cut) in
  let bound_increment = 1.0 in

  let initial_params =
    { std_dev = std_dev
    ; rects = rects
    ; tail_cut = tail_cut
    ; bound = bound_init
    ; size = size_init
    }
  in

  let search_for_size bound =
    let try_size s =
      let params = {initial_params with bound = bound; size = s} in
      let res = make_partition params in
      option_map (fun r -> {r with params = s}) res
    in
    linear_search 0.01 true try_size size_init size_cutoff size_increment
  in

  let try_bound b =
    let res = search_for_size b in
    option_map (fun r -> {r with params = b}) res
  in
  linear_search 0.01 true try_bound bound_init bound_cutoff bound_increment

let array_combine a b =
  let al = Array.to_list a in
  let bl = Array.to_list b in
  let combined = List.combine al bl in
  Array.of_list combined

let generate_ziggurat params =
  let solution = find_good_partition params.zstd_dev params.zrectangle_count (int_of_float params.ztail_cut) in
  match solution with
  | Some solution ->
     let xs, rhos = solution.point in
     let xs_ints = Array.map int_of_float xs in
     let corners = array_combine xs_ints rhos in
     Some { corners = corners
          ; params = params
          }
  | None -> None


let make_discrete_gaussian dimension std_dev rectangle_count =
  let cut = sqrt (float_of_int dimension) in
  let zigg_params =
    { zstd_dev = std_dev
    ; zrectangle_count = rectangle_count
    ; ztail_cut = ceil cut
    }
  in
  let zigg = generate_ziggurat zigg_params in
  option_map (fun z -> {zigg = z; dimension = dimension}) zigg

let sample_from_discrete_gaussian dg =
  let sample_coord _ = sample_from_ziggurat dg.zigg in
  Array.init dg.dimension sample_coord
