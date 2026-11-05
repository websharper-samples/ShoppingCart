namespace ShoppingCart

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.UI.Notation

[<JavaScript>]
module Client =

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

    type CartItem =
        {
            Item: Product
            Quantity: int
        }

    type Cart = ListModel<string, CartItem>

    let private makeItem imageSrc (title, id, price) =
        {
            Id = id
            Title = title
            Price = price
            ImageSrc = imageSrc
        }

    // The store is static data, so it lives on the client and the sample stays
    // a pure single-page app (no server round-trip, easy to host on gh-pages).
    let private store : ProductFamily list =
        let laptop = makeItem "images/laptop.png"
        let desktop = makeItem "images/desktop.png"
        let netbook = makeItem "images/netbook.png"
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
                Title = "Desktops"
                Products =
                    [
                        desktop ("Gamer 1", "id11", 699)
                        desktop ("Gamer 2", "id12", 799)
                        desktop ("Office", "id13", 599)
                        desktop ("Server", "id14", 1299)
                    ]
            }
            {
                Title = "Netbooks"
                Products =
                    [
                        netbook ("Entry", "id21", 799)
                        netbook ("Medium", "id22", 899)
                        netbook ("Cool", "id23", 699)
                        netbook ("Speed-King", "id24", 999)
                    ]
            }
        ]

    // The template is loaded from the DOM, so you can edit index.html and
    // refresh the browser without recompiling (unless you add/remove holes).
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<SPAEntryPoint>]
    let Main () =
        // Our cart is a reactive list: changes to it are immediately reflected
        // on the UI.
        let cart : Cart = ListModel.Create (fun item -> item.Item.Id) []

        IndexTemplate()
            .Title("My Shop")
            .Footer("ShoppingCart — a WebSharper.UI demo")
            .Main(
                IndexTemplate.ItemsToSell()
                    .Families(
                        store
                        |> List.map (fun family ->
                            IndexTemplate.Family()
                                .Title(family.Title)
                                .Products(
                                    family.Products
                                    |> List.map (fun product ->
                                        IndexTemplate.Product()
                                            .Title(product.Title)
                                            .Src(attr.src product.ImageSrc)
                                            .Price("$" + string product.Price)
                                            .Quantity(string 1)
                                            .AddToCart(fun e ->
                                                let qty = int e.Vars.Quantity.Value
                                                if qty > 0 then
                                                    if cart.ContainsKey(product.Id) then
                                                        let item = cart.Lens(product.Id)
                                                        item := { item.Value with Quantity = item.Value.Quantity + qty }
                                                    else
                                                        cart.Add { Item = product; Quantity = qty }
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
                                .Amount("$" + string (item.Quantity * item.Item.Price))
                                .Remove(fun _ -> cart.RemoveByKey(item.Item.Id))
                                .Increment(fun _ ->
                                    cart.Lens(item.Item.Id) := { item with Quantity = item.Quantity + 1 }
                                )
                                .Decrement(fun _ ->
                                    if item.Quantity <= 1 then
                                        cart.RemoveByKey(item.Item.Id)
                                    else
                                        cart.Lens(item.Item.Id) := { item with Quantity = item.Quantity - 1 }
                                )
                                .Doc()
                        )
                    )
                    .TotalAmount(
                        cart.View
                        |> View.Map (Seq.sumBy (fun item -> item.Item.Price * item.Quantity))
                        |> View.Map (fun total -> "$" + string total)
                    )
                    .Checkout(fun _ -> JS.Alert("Checkout was called!"))
                    .EmptyCart(fun _ -> cart.Clear())
                    .Doc()
            )
            .Bind()
