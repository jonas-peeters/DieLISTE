import PostgreSQLProvider
import FluentProvider

extension Config {
    public func setup() throws {
        // allow fuzzy conversions for these types
        // (add your own types here)
        Node.fuzzy = [Row.self, JSON.self, Node.self]

        try setupProviders()
        try setupPreparations()
        print("Test email incoming")
        print(sendEMailVerificationEMail(email: "jonas.peeters@icloud.com", username: "Admin", link: "https://die-liste.herokuapp.com/user/verify/test", config: self))
    }
    
    /// Configure providers
    private func setupProviders() throws {
        try addProvider(FluentProvider.Provider.self)
        try addProvider(PostgreSQLProvider.Provider.self)
    }
    
    /// Add all models that should have their
    /// schemas prepared before the app boots
    private func setupPreparations() throws {
        preparations.append((User.self))
        preparations.append(List.self)
        preparations.append(Supermarket.self)
        preparations.append(Category.self)
        preparations.append(Item.self)
        preparations.append(Pivot<User, List>.self)
        preparations.append(Pivot<User, Supermarket>.self)
        preparations.append(Pivot<Category, Supermarket>.self)
    }
}
