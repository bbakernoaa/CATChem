/**
 * @file catchem_musica_registration.cpp
 * @brief Register optional MUSICA processes when their libraries are linked.
 */

#if defined(CATCHEM_BUILD_MUSICA)
extern "C" void catchem_register_photolysis_cpp();
extern "C" void catchem_register_gaschem_cpp();
#endif

/**
 * @brief Register Photolysis and GasChem when CATChem was built with MUSICA.
 *
 * The strong references in this optional translation unit also ensure static
 * linkers extract both process objects from their archives.
 */
extern "C" void catchem_register_musica_cpp() {
#if defined(CATCHEM_BUILD_MUSICA)
    catchem_register_photolysis_cpp();
    catchem_register_gaschem_cpp();
#endif
}
