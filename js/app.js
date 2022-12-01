console.log("Hello, world!");

class Hello extends React.Component {
    render() {
        return <p>Hello {this.props.name}</p>;
    };
}

class AxiomsColumn extends React.Component {
    render () {
        console.log("Rendering AxiomsColumn");
        return <div>
            <h3 className="title is-3">Axioms</h3>
            <div className="block">
            <label className="label">Current axiom set</label>
                Empty
              </div>
              <div className="block">
                <label className="label">Add a new equational axiom</label>
                <div className="field has-addons">
                  <div className="control">
                    <input className="input" type="text" placeholder="expr1 = expr2"></input>
                  </div>
                  <div className="control">
                    <a className="button is-info">
                      Add
                    </a>
                  </div>
                </div>
            <p className="help">Write a new axiom in the form expr1 = expr2</p>
            </div>
            </div>;
    };
}

ReactDOM.render(<AxiomsColumn />, document.getElementById('axioms-root'));
ReactDOM.render(<Hello name='World'/>, document.getElementById('goal-root'));
