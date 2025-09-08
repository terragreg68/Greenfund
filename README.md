# 🌱 Greenfund - Decentralized Green Project Funding DAO

A blockchain-based Decentralized Autonomous Organization (DAO) built on Stacks for funding eco-friendly startups and green projects. Community members stake STX tokens to gain voting power and collectively decide which environmental projects deserve funding.

## 🌍 Features

- **🏛️ DAO Membership**: Stake STX tokens to join and gain voting power
- **📝 Project Submissions**: Eco-entrepreneurs can submit green projects for funding
- **🗳️ Democratic Voting**: Members vote on project proposals based on their stake
- **💰 Crowdfunding**: Approved projects receive community funding
- **🔒 Secure Withdrawals**: Project creators can withdraw funds once goals are met
- **📊 Transparent Tracking**: All votes, funding, and project status are on-chain

## 🚀 Getting Started

### Prerequisites
- Clarinet CLI installed
- Stacks wallet with STX tokens

### Installation

```bash
git clone <repository-url>
cd greenfund
clarinet check
```

## 📖 Usage Guide

### 1. 🤝 Join the DAO
```clarity
(contract-call? .greenfund join-dao u10000000) ;; Stake 10 STX
```

### 2. 📋 Submit a Green Project
```clarity
(contract-call? .greenfund submit-project 
  "Solar Community Garden" 
  "Installing solar panels for urban farming initiative" 
  u50000000) ;; 50 STX funding goal
```

### 3. 🗳️ Vote on Projects
```clarity
(contract-call? .greenfund vote-on-project u1 true) ;; Vote YES on project #1
```

### 4. ⏰ Finalize Voting
```clarity
(contract-call? .greenfund finalize-voting u1) ;; After voting period ends
```

### 5. 💵 Fund Approved Projects
```clarity
(contract-call? .greenfund fund-project u1 u5000000) ;; Fund 5 STX
```

### 6. 💸 Withdraw Funds (Project Creators)
```clarity
(contract-call? .greenfund withdraw-funds u1) ;; Withdraw when goal met
```

## 🔍 Read-Only Functions

- `get-project`: View project details
- `get-member-info`: Check member stake and voting power
- `get-dao-treasury`: View total DAO funds
- `get-project-vote`: Check how someone voted
- `get-backing-amount`: See funding contributions

## 🏗️ Project Lifecycle

1. **📝 Submission** → Project submitted by creator
2. **🗳️ Voting** → DAO members vote (1008 blocks period)
3. **✅ Approved/❌ Rejected** → Based on vote results
4. **💰 Funding** → Community funds approved projects
5. **🎯 Funded** → Goal reached, ready for withdrawal
6. **✨ Completed** → Funds withdrawn by creator

## ⚙️ Configuration

- **Voting Period**: 1008 blocks (~1 week)
- **Minimum Votes**: 10 votes required
- **Voting Power**: 1 vote per 1000 STX staked

## 🛡️ Security Features

- Only project creators can withdraw their funds
- Voting limited to DAO members with stake
- One vote per member per project
- Time-locked voting periods
- Transparent fund tracking

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Test with Clarinet
4. Submit a pull request

## 📄 License

MIT License - Build the green future! 🌱

---

*Empowering sustainable innovation through decentralized funding* 🌍💚
```

