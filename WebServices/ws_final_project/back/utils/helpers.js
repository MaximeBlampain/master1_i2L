module.exports={
    getRouteFromRequest,
    logging,
}

function getRouteFromRequest(request){
    if(request && request.route && request.route.path) 
        return request.route.path
    return 'UNKNOWN ROUTE'
}

function logging(route, response){
    if(!response || !response.type)
        console.error(`[route] '${route}' - No type of response`);
    else {
        if(response.type == 'success'){
            if(response.message)
                console.info(`[route] '${route}' - SUCCESS - ${response.message}`)
            else
                console.info(`[route] '${route}' - SUCCESS - Request completed`)
        }
        else {
            if(response.message)
                console.error(`[route] '${route}' - ERROR - ${response.message}`)
            else
                console.error(`[route] '${route}' - ERROR - Request failed`)
        } 
    }
}