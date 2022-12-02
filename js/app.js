class Hello extends React.Component {
    render() {
        return <p>Hello {this.props.name}</p>;
    };
}

class Axiom extends React.Component {
    constructor(props) {
        super(props);
    };

    render() {
        return(<li> <div className="axiom_name">{this.props.name}</div> {this.props.left} = {this.props.right} </li>);
    };
}

class Axioms extends React.Component {
    constructor(props) {
        super(props);
        this.state = { axioms: this.props.axioms };
        this.handleAddAxiom = this.handleAddAxiom.bind(this);
    };

    handleAddAxiom(new_axiom) {
        let left = new_axiom.expr.split('=')[0];
        let right = new_axiom.expr.split('=')[1];
        this.setState((oldState) => {
            return {axioms: oldState.axioms.concat({name:new_axiom.name, left:left, right:right})}
        });
    }

    render () {
        return (
            <div>
            <h3 className="title is-3">Axioms</h3>
            <AxiomsList axioms={this.state.axioms} />
            <AxiomForm onAddAxiom={this.handleAddAxiom} />
            </div>);
    };
}

class AxiomsList extends React.Component {
    constructor(props) {
        super(props);
    };
    render() {
        let e_display;
        let e_axioms;
        if (!this.props.axioms.length) {
            e_display = <p>Axiom set is empty. Add some equalities to get started!</p>
        } else {
            e_axioms = this.props.axioms.map ((axiom) => <Axiom {...axiom} />);
            e_display = <ol className="axioms_list">{e_axioms}</ol>
        }
        return (
        <div className="block">
        <label className="label title is-5">Current axiom set</label>
        {e_display}
        </div>);
    };
}

class AxiomForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {name: '', expr: ''};
    this.handleNameChange = this.handleNameChange.bind(this);
    this.handleExprChange = this.handleExprChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleNameChange(event) {
    this.setState({name: event.target.value});
  }

  handleExprChange(event) {
    this.setState({expr: event.target.value});
  }

  handleSubmit(event) {
    event.preventDefault();
    console.log('Axiom form submitted with values: ' + this.state.name + ' and ' + this.state.expr);
    this.props.onAddAxiom({name: this.state.name, expr: this.state.expr});
    this.setState({name:'', expr:''});
  }

  render() {
    return (
    <form className="block" onSubmit={this.handleSubmit}>
    <label className="label">Add a new equational axiom</label>
    <div className="field has-addons">
      <div className="control">
        <input className="input" type="text" placeholder="name" value={this.state.name} onChange={this.handleNameChange}></input>
      </div>
      <div className="control">
        <input className="input" type="text" placeholder="expr1 = expr2" value={this.state.expr} onChange={this.handleExprChange}></input>
      </div>
      <div className="control">
        <button type="submit" className="button is-info">Add</button>
      </div>
    </div>
    <p className="help">Add an axiom in the form expr1 = expr2. Make sure to give it a meaningful name. </p>
    </form>
    );
  }
}

const axiom1 = {name: "ax1", left: "hello", right: "world" };
const axiom2 = {name: "ax2", left: "goodbye", right: "world" };
const default_axioms = [axiom1, axiom2];
ReactDOM.render(<Axioms axioms={[]} />, document.getElementById('axioms-root'));
ReactDOM.render(<Hello name='World'/>, document.getElementById('goal-root'));
