import MySQL
import Vapor

/// # Connect to MySQL Database
///
/// Using this function the server connects to a MySQL database by the information
/// provided through the configuration file
func connectToDatabase(withConfig config: Config) -> Connection {
    
    let dbhostname = config["hostname"]!.string!
    let dbuser = config["user"]!.string!
    let dbpassword = config["password"]!.string!
    let dbdatabase = config["database"]!.string!
    
    var connection: Connection?
    
    do {
        let mysql = try Database(hostname: dbhostname,
                             user: dbuser,
                             password: dbpassword,
                             database: dbdatabase)
        print("Connecting to DB...")
        connection = try mysql.makeConnection()
        print("Connected!")
        try print("DB Version: \(connection!.execute("SELECT @@version").wrapped.array![0])")
    } catch {
        print("Unable to connect to MySQL:  \(error)")
    }
    
    return connection!
}
