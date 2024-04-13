/* Counter.res */

/* Define a module Counter */
module Counter = {
  /* Define a mutable counter variable */
  let counter = ref(0)

  /* Function to increment the counter */
  let increment = () => {
    counter := counter + 1
  }

  /* Function to decrement the counter */
  let decrement = () => {
    counter := counter - 1
  }

  /* Function to get the current value of the counter */
  let getValue = () => {
    !counter
  }
}

/* Example usage */
let () = {
  /* Increment the counter */
  Counter.increment()

  /* Print the current value of the counter */
  Js.log("Current value of counter:", Counter.getValue())

  /* Decrement the counter */
  Counter.decrement()

  /* Print the current value of the counter */
  Js.log("Current value of counter:", Counter.getValue())
}
