var defaultHeaders = {
    "Accept": "application/json",
    "Content-Type": "application/json; charset=UTF-8",
}
  


export const getCors = (specificHeaders) => {
    var headers = (specificHeaders !== undefined) ? {...defaultHeaders,...specificHeaders} : {...defaultHeaders}
    return {
        method: "GET",
        headers: new Headers(headers),
    }
}

export const postCors = (body, specificHeaders) => {
    var headers = (specificHeaders !== undefined) ? {...defaultHeaders, ...specificHeaders} : {...defaultHeaders}
    return {
        method: "POST",
        headers: new Headers(headers),
        body: JSON.stringify(body),
    }
}

export const deleteCors = (body={}, specificHeaders) => {
    var headers = (specificHeaders !== undefined) ? {...defaultHeaders, ...specificHeaders} : {...defaultHeaders}
    return {
        method: "DELETE",
        headers: new Headers(headers),
        body: JSON.stringify(body),
    }
}

export const putCors = (body, specificHeaders) => {
    var headers = (specificHeaders !== undefined) ? {...defaultHeaders, ...specificHeaders} : {...defaultHeaders}
    return {
        method: "PUT",
        headers: new Headers(headers),
        body: JSON.stringify(body),
    }
}

export const patchCors = (body, specificHeaders) => {
    var headers = (specificHeaders !== undefined) ? {...defaultHeaders, ...specificHeaders} : {...defaultHeaders}
    return {
        method: "PATCH",
        headers: new Headers(headers),
        body: JSON.stringify(body),
    }
}
