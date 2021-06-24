object NBDWord2 {

  var person1 = new Person("Erwin", "Smith")
  var person2 = new Person("John", "White")
  var person3 = new Person("Eddie", "Brown")

  val student = new Person("AAA", "BBBB") with Student
  println(s"students tax: ${student.tax}%")

  val worker = new Person("Worker1", "Worker1") with Worker
  println(s"worker tax: ${worker.tax}%")

  val teacher = new Person("Teacher1", "Teacher1") with Teacher
  println(s"teacher tax: ${teacher.tax}%")

  val studentWorker = new Person("SSSS", "SSSS") with Student with Worker
  println(s"studentWorker tax: ${studentWorker.tax}%")

  def task1(str: String): String = {
    val weekDays = List("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    val weekendDays = List("Saturday", "Sunday")

    str match {
      case a if (weekDays.contains(a)) => "Job"
      case b if (weekendDays.contains(b)) => "Weekend"
      case _ => "No such a day"
    }
  }

  def task3(p: Person) = {
    val greeting = p match {
      case Person("Erwin", "Smith") => "Good to see you Erwin"
      case Person("John", "White") => "Greetings John"
      case Person("Eddie", "Brown") => "Welcome Eddie"
      case _ => "Hello world."
    }
    greeting
  }

  def task4(number: Int, func: (Int) => Int): Int = {
    func(func(func(number)))
  }
}
