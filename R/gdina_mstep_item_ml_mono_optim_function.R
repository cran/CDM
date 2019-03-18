## File Name: gdina_mstep_item_ml_mono_optim_function.R
## File Version: 0.10


gdina_mstep_item_ml_mono_optim_function <- function()
{
    fun <- cdm_attach_internal_function(pack="ROI", fun="nlminb2")
    return(fun)
}
