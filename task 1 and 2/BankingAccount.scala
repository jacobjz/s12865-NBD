class BankingAccount(state: Double) {
  private var _accountState: Double = state

  def accountState: Double = _accountState

  def this() {
    this(0)
  }

  def deposit(amount: Double): Double = {
    _accountState += amount
    _accountState
  }

  def withdraw(amount: Double): Double = {
    if (_accountState - amount < 0){
      return _accountState
    }
    _accountState = accountState - amount
    _accountState
  }
}