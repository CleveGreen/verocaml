Exact-checkout hygiene is a separate Git-required mode with literal product and
package roots and stable dirt categories.

  $ python3 hygiene_tests.py
  checkout-hygiene clean=1 target/tracked/staged=3 untracked-product-roots=11
  checkout-hygiene non-product-dirt=3 missing-repository/object=passed
