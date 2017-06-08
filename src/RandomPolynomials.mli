val sample_coefficient : int -> Polynomial.coefficient
val sample_bounded_poly : int -> int -> Polynomial.t

type probability = float
type ziggurat_params =
  { zstd_dev : float
  ; ztail_cut : float
  ; zrectangle_count : int
  }
type ziggurat =
  { corners : (int * probability) array
  ; params : ziggurat_params
  }


val generate_ziggurat : ziggurat_params -> ziggurat option
val sample_from_ziggurat : ziggurat -> int

val gaussian_pdf : float -> float -> float
val gaussian_pdf_inverse : float -> float -> float
