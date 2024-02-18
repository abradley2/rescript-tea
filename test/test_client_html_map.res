open Tea.App
open Tea.Html

type countUpMsg = Increment
type countDownMsg = Decrement

type page =
  | CountUp
  | CountDown

type msg =
  | SetPage(page)
  | CountUpMsg(countUpMsg)
  | CountDownMsg(countDownMsg)

type model = (int, page)

let init = () => (CountDown, 0)

let update = ((page, count), msg) => {
  switch msg {
  | SetPage(newPage) => (newPage, count)
  | CountUpMsg(subMsg) =>
    switch subMsg {
    | Increment => (page, count + 1)
    }
  | CountDownMsg(subMsg) =>
    switch subMsg {
    | Decrement => (page, count - 1)
    }
  }
}

let viewButton = (~title, ~disabled=false, ~msg, ()) =>
  button(list{Events.onClick(msg), Attributes.disabled(disabled)}, list{text(title)})

let view = ((page, count)) => div(list{},
  list{
    h3(list{}, list{text("Count: " ++ string_of_int(count))}),
    viewButton(~title="Count Up", ~msg=SetPage(CountUp), ~disabled=page == CountUp, ()),
    viewButton(~title="Count Down", ~msg=SetPage(CountDown), ~disabled=page == CountDown, ()),
    switch page {
    | CountUp => Tea.Html.map(msg => CountUpMsg(msg), viewButton(~title="Increment", ~msg=Increment, ()))
    | CountDown => Tea.Html.map(msg => CountDownMsg(msg), viewButton(~title="Decrement", ~msg=Decrement, ()))
    },
  }
)

let main = beginnerProgram({ model: init(), update, view })