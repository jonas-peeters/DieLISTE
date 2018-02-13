import Vapor
import JSON

/// # Return Messages
/// Generates a return message from a code
///
/// ## Codes:
/// ### Good:
///     10: Success
///     11: Autheticated
///     12: Accepted
///
///
/// ### Bad:
///     20: JSON not found
///     21: Parameter not found
///     22: User not found
///     23: List not found
///     24: Item not found
///     25: JSON malformed
///
///     30: Could not read
///     31: Could not write
///     32: E-Mail not sent
///
///     40: Not authenticated
///     41: Username not found
///     42: E-Mail not found
///     43: Password/E-Mail incorrect
///     44: Username or e-mail unavailable
///     45: E-Mail not verified
///
///
/// - Parameter code: Status code
/// - Returns: A response
func status(_ code: Int) -> Response {
    do {
        switch code {
        case 10:
            return try Response(status: .ok, json:
                createStatus(10, "Success")
            )
        case 11:
            return try Response(status: .ok, json:
                createStatus(11, "Autheticated")
            )
        case 12:
            return try Response(status: .ok, json:
                createStatus(12, "Accepted")
            )
            
        case 20:
            return try Response(status: .badRequest, json:
                createStatus(20, "JSON not found", ["Format body as application/json.","Make sure syntax is correct."])
            )
        case 21:
            return try Response(status: .badRequest, json:
                createStatus(21, "Parameter not found", ["Make sure your request complies to the request requirements.", "Make sure there are no typos."])
            )
        case 22:
            return try Response(status: .badRequest, json:
                createStatus(22, "User not found", ["The user your request requires cannot be found. Make sure that the provided id/username is valid. You can check this by logging in as said user and request the user data with a GET request to /user/", "Make sure there are no typos."])
            )
        case 23:
            return try Response(status: .badRequest, json:
                createStatus(23, "List not found", ["The user may not have access to this list.", "The list may not exist.", "Make sure there are no typos and the list id is correct."])
            )
        case 24:
            return try Response(status: .badRequest, json:
                createStatus(24, "Item not found", ["The user may not have access to this item.", "The item may not exist.", "Make sure there are no typos and the item id is correct."])
            )
        case 25:
            return try Response(status: .badRequest, json:
                createStatus(25, "JSON malformed", ["Make sure there are no typos.", "Make sure all required keys are set.", "Make sure are values are of the correct type."])
            )
            
        case 30:
            return try Response(status: .internalServerError, json:
                createStatus(30, "Could not read from database", ["This is most likely not your fault. Please contact the server admin for further guidiance."])
            )
        case 31:
            return try Response(status: .internalServerError, json:
                createStatus(31, "Could not write to database", ["This is most likely not your fault. Please contact the server admin for further guidiance."])
            )
        case 32:
            return try Response(status: .internalServerError, json:
                createStatus(32, "E-Mail not sent", ["This is most likely not your fault. Please contact the server admin for further guidiance."])
            )
            
        case 40:
            return try Response(status: .badRequest, json:
                createStatus(40, "Not autheticated", ["Try to re-login.", "Make sure you have the permission to access the requested resource."])
            )
        case 41:
            return try Response(status: .badRequest, json:
                createStatus(41, "Username not found", ["Make sure there is no typo in your username.", "This user may not exist."])
            )
        case 42:
            return try Response(status: .badRequest, json:
                createStatus(42, "E-Mail not found", ["Make sure there is no typo in your e-mail.", "A user with this e-mail may not exist."])
            )
        case 43:
            return try Response(status: .badRequest, json:
                createStatus(43, "Password/E-Mail incorrect", ["Make sure there is no typo in your password/e-mail.", "Try to reset your password by sending a POST request to /user/password/forgot with your e-mail address in a json body. For info check the server documentation about sending forgot password requests."])
            )
        case 44:
            return try Response(status: .badRequest, json:
                createStatus(44, "Username or e-mail unavailable", ["Use a different e-mail.", "Use a different username."])
            )
        case 45:
            return try Response(status: .badRequest, json:
                createStatus(45, "E-Mail not verified", ["Verify your e-mail from the profil page."])
            )
        default:
            return try Response(status: .imATeapot, json:
                createStatus(-1, "Default response", ["You should never get this response. Please contact the server admin."])
            )
        }
    } catch {
        return Response(status: .internalServerError)
    }
}

/// Helper for creating the response messages
///
/// - Parameters:
///   - code: The status code
///   - message: The message describing the status
/// - Returns: JSON encoded status
func createStatus(_ code: Int, _ message: String) -> JSON {
    var status = JSON()
    do {
        try status.set("code", code)
        try status.set("message", message)
    } catch {
        print("Error at generating status messages")
    }
    return status
}

/// Helper for creating the response messages
///
/// - Parameters:
///   - code: The status code
///   - message: The message describing the status
///   - solutions: Possible solutions for the problem
/// - Returns: JSON encoded status
func createStatus(_ code: Int, _ message: String, _ solutions: [String]) -> JSON {
    var status = JSON()
    do {
        try status.set("code", code)
        try status.set("message", message)
        try status.set("solutions", solutions)
    } catch {
        print("Error at generating status messages")
    }
    return status
}
