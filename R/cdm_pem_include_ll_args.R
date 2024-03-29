## File Name: cdm_pem_include_ll_args.R
## File Version: 0.032

cdm_pem_include_ll_args <- function(ll_args, pem_parm, pem_pars, pem_parameter_index)
{
    for (pp in pem_pars){
        ll_args[[ pp ]] <- cdm_pem_extract_parameters( parm=pem_parm, parmgroup=pp,
                                pem_parameter_index=pem_parameter_index )
    }
    return(ll_args)
}
