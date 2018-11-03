      module thread_priv_mod
	integer, prameter :: n = 3
	integer m(n)
	!$OMP threadprivate(m)
      end module thread_priv_mod


