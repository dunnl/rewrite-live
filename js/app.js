console.log("Hello, world!");

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

class AxiomsColumn extends React.Component {
    constructor(props) {
        super(props);
        this.state = { axioms: this.props.axioms };
    };
    render () {
        console.log("Rendering AxiomsColumn");
        let e_axioms;
        if (!this.state.axioms) {
            e_axioms = <p>Axiom set is empty. Add some equalities to get started!</p>
        } else {
            e_axioms = this.state.axioms.map ((axiom) => <Axiom {...axiom} />);
        }
        console.log(e_axioms);
        return (
            <div>
            <h3 className="title is-3">Axioms</h3>
              <div className="block">
                <label className="label">Current axiom set</label>
                <ol class="axioms_list">{e_axioms}</ol>
              </div>
              <div className="block">
                <label className="label">Add a new equational axiom</label>
                <div className="field has-addons">
                <div className="control">
                    <input className="input" type="text" placeholder="name"></input>
                </div>
                <div className="control">
                    <input className="input" type="text" placeholder="expr1 = expr2"></input>
                </div>
                <div className="control">
                  <a className="button is-info">
                    Add
                  </a>
                </div>
              </div>
                <p className="help">Add an axiom in the form expr1 = expr2. Make sure to give it a meaningful name. </p>
            </div>
            </div>);
    };
}

const axiom1 = {name: "ax1", left: "hello", right: "world" };
const axiom2 = {name: "ax2", left: "goodbye", right: "world" };

ReactDOM.render(<AxiomsColumn axioms={[axiom1, axiom2]} />, document.getElementById('axioms-root'));
ReactDOM.render(<Hello name='World'/>, document.getElementById('goal-root'));
