module Counter = {
  let counter = ref(
    0,
  )

  let increment = () => {
    counter :=
      counter + 1
  }

  let decrement = () => {
    counter :=
      counter - 1
  }

  let getValue = () => {
    !counter
  }
}

let () = {
  Counter.increment()

  Js.log(
    "Current value of counter:",
    Counter.getValue(),
  )

  Counter.decrement()

  Js.log(
    "Current value of counter:",
    Counter.getValue(),
  )
}
