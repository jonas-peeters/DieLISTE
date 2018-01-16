import Vapor
import AuthProvider
import Sessions
import HTTP

extension Droplet {
    /// # Setting up Routes
    func setupRoutes() throws {
        
        let memory = MemorySessions()
        let passwordMiddleware = PasswordAuthenticationMiddleware(User.self)
        let persistMiddleware = PersistMiddleware(User.self)
        let sessionsMiddleware = SessionsMiddleware(memory)
        
        let authedRoute = grouped([sessionsMiddleware, persistMiddleware, passwordMiddleware])
        let loginRoute = grouped([sessionsMiddleware, persistMiddleware])
        
        let infoController = InfoController()
        infoController.addRoutes(drop: self)
        
        let userController = UserController(view)
        userController.addRoutes(drop: self, loginRoute: loginRoute, authedRoute: authedRoute)
    }
}
