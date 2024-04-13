interface IUser {
    id: string;
    name: string;
    age: number;
  }
  
  interface IAdminUser extends IUser {
    permissions: string[];
  }
  interface IProduct {
    id: string;
    name: string;
    price: number;
    description?: string; // optional property
    applyDiscount(discountCode: string): number; // method
  }
  
  class User implements IUser {
    constructor(public id: string, public name: string, public age: number) {}
    
    greet() {
      console.log(`Hello, my name is ${this.name}`);
    }
  }
  
function check(a : string) : number {
  console.log(a);
  return 5;
}

type IPurs = {
  name : string,
  org : string,
  rank : number
}