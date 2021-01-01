namespace MyShoppingCart

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Notation

[<JavaScript>]
module DTO =
    type Product =
        {
            Id: string
            Title: string
            Price: int
            ImageSrc: string
        }

    type ProductFamily =
        {
            Title: string
            Products: Product list
        }
    
    type Store = ProductFamily list

module Server =
    open DTO

    [<Rpc>]
    let GetInfo () =
        let item imageSrc (title, id, price) =
            {
                Id = id
                Title = title
                Price = price
                ImageSrc = imageSrc
            }
        let laptop product = item "/images/laptop.png" product
        let desktop product = item "/images/desktop.png" product
        let netbook product = item "/images/netbook.png" product
        [
            {
                Title = "Laptops"
                Products =
                    [
                        laptop ("Toshiba", "id1", 1299)
                        laptop ("HP", "id2", 1499)
                        laptop ("Dell", "id3", 1499)
                        laptop ("Acer", "id4", 1499)
                    ]
            }
            {
                Title = "Laptops"
                Products =
                    [
                        desktop ("Gamer 1", "id11", 699)
                        desktop ("Gamer 2", "id12", 799)
                        desktop ("Office", "id13", 599)
                        desktop ("Server", "id14", 1299)
                    ]
            }
            {
                Title = "Laptops"
                Products =
                    [
                        netbook ("Entry", "id21", 799)
                        netbook ("Medium", "id22", 899)
                        netbook ("Cool", "id23", 699)
                        netbook ("Speed-King", "id24", 999)
                    ]
            }
        ]

[<JavaScript>]
module Client =
    open DTO
    
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    type CartItem =
        {
            Item: Product
            Quantity: int
        }

    // Our cart is a reactive list - changes to it
    // are immediately reflected on the UI.
    type Cart = ListModel<string, CartItem>

    // Populate our store. This can be changed to fetch from a
    // server if needed.
    let store =
        let item imageSrc (title, id, price) =
            {
                Id = id
                Title = title
                Price = price
                ImageSrc = imageSrc
            }
        let laptop product = item "/images/laptop.png" product
        let desktop product = item "/images/desktop.png" product
        let netbook product = item "/images/netbook.png" product
        [
            {
                Title = "Laptops"
                Products =
                    [
                        laptop ("Toshiba", "id1", 1299)
                        laptop ("HP", "id2", 1499)
                        laptop ("Dell", "id3", 1499)
                        laptop ("Acer", "id4", 1499)
                    ]
            }
            {
                Title = "Laptops"
                Products =
                    [
                        desktop ("Gamer 1", "id11", 699)
                        desktop ("Gamer 2", "id12", 799)
                        desktop ("Office", "id13", 599)
                        desktop ("Server", "id14", 1299)
                    ]
            }
            {
                Title = "Laptops"
                Products =
                    [
                        netbook ("Entry", "id21", 799)
                        netbook ("Medium", "id22", 899)
                        netbook ("Cool", "id23", 699)
                        netbook ("Speed-King", "id24", 999)
                    ]
            }
        ]
        |> ignore
        Server.GetInfo()

    // Set up empty cart
    let cart : Cart = ListModel.Create (fun item -> item.Item.Id) []

    [<SPAEntryPoint>]
    let Main () =
        IndexTemplate()
            .Title("My Shop")
            .Footer("MyShoppingCart - a simple WebSharper.UI demo app")
            .Main(
                IndexTemplate.ItemsToSell()
                    .Families(
                        store
                        |> List.map (fun family ->
                            IndexTemplate.Family()
                                .Title("Laptops")
                                .Products(
                                    family.Products
                                    |> List.map (fun product ->
                                        IndexTemplate.Product()
                                            .Title(product.Title)
                                            .Price(string product.Price)
                                            .ImageSrc(product.ImageSrc)
                                            .Quantity(string 1)
                                            .AddToCart(fun e ->
                                                if cart.ContainsKey(product.Id) then
                                                    let item = cart.Lens(product.Id)
                                                    let quantity = item.LensAuto (fun item -> item.Quantity)
                                                    quantity := quantity.Value + int e.Vars.Quantity.Value
                                                elif int e.Vars.Quantity.Value <> 0 then
                                                    cart.Add
                                                        {
                                                            Item = product
                                                            Quantity = int e.Vars.Quantity.Value
                                                        }
                                                else ()
                                            )
                                            .Doc()
                                    )
                                )
                                .Doc()
                        )
                    )
                    .Doc()
            )
            .Sidebar(
                IndexTemplate.ShoppingCart()
                    .CartItems(
                        cart.View.DocSeqCached (fun item ->
                            IndexTemplate.CartItem()
                                .Name(item.Item.Title)
                                .Quantity(string item.Quantity)
                                .Amount(string (item.Quantity * item.Item.Price))
                                .Remove(fun e ->
                                    cart.RemoveByKey(item.Item.Id)
                                )
                                .Increment(fun e ->
                                    cart.Lens(item.Item.Id) := { item with Quantity=item.Quantity+1 }
                                )
                                .Decrement(fun e ->
                                    if item.Quantity <= 1 then
                                        cart.RemoveByKey(item.Item.Id)
                                    else
                                        cart.Lens(item.Item.Id) := { item with Quantity=item.Quantity-1 }
                                )
                                .Doc()
                        )
                    )
                    .TotalAmount(
                        cart.View
                        |> View.Map (Seq.sumBy (fun item -> item.Item.Price * item.Quantity))
                        |> View.Map string
                    )
                    .Checkout(fun _ -> JS.Alert("Checkout was called!"))
                    .EmptyCart(fun _ ->
                        cart.Clear()
                    )
                    .Doc()
            )
            .Bind()
